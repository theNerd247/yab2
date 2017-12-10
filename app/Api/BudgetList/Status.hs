module Api.BudgetList.Status (resource) where

import           Api.ApiTypes
import           Api.BudgetList             (WithBudgetList)
import           Api.ByRange
import           Control.Error.Util         ((!?), (??))
import           Control.Lens               hiding ((!?), (??))
import           Control.Monad              (forM)
import           Control.Monad.Reader       (MonadReader, ReaderT (..), ask,
                                             asks)
import           Control.Monad.Trans        (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Data.Aeson
import           Data.Budget
import           Data.Data
import           Data.IxSet
import           Data.JSON.Schema           hiding (Proxy)
import qualified Data.List                  as DL
import           Data.Maybe                 (listToMaybe, maybeToList)
import qualified Data.Text                  as T
import           GHC.Generics               hiding (to)
import           Rest
import           Rest.Dictionary.Types
import           Rest.Handler
import qualified Rest.Resource              as R
import           Rest.Types.Info
import           YabAcid

type SID = Day

data MID = All | ByRange

type WithStatus = ReaderT SID WithBudgetList

instance Info Day where
  describe _ = "day"

resource :: Resource WithBudgetList WithStatus SID MID Void
resource = mkResourceReader
  { R.name = "status"
  , R.schema = withListing All $ named [ ("on", singleBy read ), ("between", listing ByRange) ]
  , R.get = Just get
  , R.list = list
  }

mkParamHandler dict h = mkHandler dict $ \env -> ask >>= h (param env)

list :: MID -> ListHandler WithBudgetList
list All     = listAll
list ByRange = listByDayRange

get :: Handler WithStatus
get = mkHandler (jsonI . jsonO) $ \env -> do
  name <- (lift . lift) ask
  db <- (lift . lift . lift) (asks $ view db)
  date <- ask
  let itemsFilter = fmap T.unpack . maybeToList $ input env 
  bs <- asYabList db name (getBudgetByName db name)   !? NotAllowed
  es <- asYabList db name (getExpensesByName db name) !? NotAllowed
  let budget = filterBudgetItems itemsFilter bs
  let expenses = filterBudgetItems itemsFilter es
  return $ compareBudgetsOn (dayToRate (budget^.startDate) $ dayToDate date) budget expenses

listByDayRange :: ListHandler WithBudgetList
listByDayRange = mkCustomListing (dayRangeParam . jsonO) $ \env -> do
  db <- (lift . lift) (ask $ view db)
  name <- lift ask
  let stime = dayToDate . sdate $ param env
  let etime = dayToDate . edate $ param env
  budget <- asYabList db name (getBudgetByName db name) !? NotAllowed
  expenses <- asYabList db name (getExpensesByName db name) !? NotAllowed
  let sp = (dayToRate (budget^.startDate) stime)
  let ep = (dayToRate (budget^.startDate) etime)
  let cs = compareBudgetsBetween sp ep  budget expenses
  return $ (DL.sortOn (view _1) cs) & traverse . _1 %~ (periodToDay $ budget^.startDate)

listAll :: ListHandler WithBudgetList
listAll = mkListing jsonO $ \range -> do
  name <- ask
  db <- (lift . lift) (ask $ view db)
  budget <- asYabList db name (getBudgetByName db name) !? NotAllowed
  expenses <- asYabList db name (getExpensesByName db name) !? NotAllowed
  let cs = compareBudgetsBetween (offset range) (offset range + count range) budget expenses
  return $ (DL.sortOn (view _1) cs) & traverse . _1 %~ periodToDay (budget^.startDate)
