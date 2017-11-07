{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Api.BudgetList.Status (resource) where

import Api.ApiTypes
import Api.BudgetList (WithBudget)
import Api.ByRange
import Control.Error.Util ((??),(!?))
import Control.Lens hiding ((??),(!?))
import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks, ask)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson
import Data.Budget
import Data.Data
import Data.Maybe (listToMaybe)
import Data.IxSet
import Data.JSON.Schema hiding (Proxy)
import GHC.Generics hiding (to)
import Rest
import Rest.Dictionary.Types
import Rest.Handler
import Rest.Types.Info
import YabAcid
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Rest.Resource as R

type SID = Day

data MID = All | ByRange 

type WithStatus = ReaderT SID WithBudget

instance Info Day where
  describe _ = "day"

resource :: Resource WithBudget WithStatus SID MID Void
resource = mkResourceReader
  { R.name = "status"
  , R.schema = withListing All $ named [ ("on", singleBy read ), ("between", listing ByRange) ] 
  , R.get = Just get
  , R.list = list
  }

mkParamHandler dict h = mkHandler dict $ \env -> ask >>= h (param env)

list :: MID -> ListHandler WithBudget
list All = listAll
list ByRange = listByDayRange

get :: Handler WithStatus
get = mkIdHandler jsonO $ handler
  where
    handler :: () -> SID -> ExceptT Reason_ WithStatus (Maybe BudgetStatusItem)
    handler _ date = do 
      name <- (lift . lift) ask
      db <- (lift . lift . lift) (asks $ view db)
      budget <- (asYabList db name $ getBudgetByName db name) !? NotAllowed
      expenses <- (asYabList db name $ getExpensesByName db name) !? NotAllowed
      let cs = compareBudgetsOn (dayToRate (budget^.startDate) $ dayToDate date) budget expenses 
      return $ listToMaybe $ [cs & _1 %~ (rateToDay $ budget^.startDate)]

listByDayRange :: ListHandler WithBudget
listByDayRange = mkCustomListing (dayRangeParam . jsonO) $ \env -> do
  db <- (lift . lift) (ask $ view db)
  name <- (lift) ask
  let stime = dayToDate . sdate $ param env
  let etime = dayToDate . edate $ param env
  budget <- (asYabList db name $ getBudgetByNameAndDates db name stime etime) !? NotAllowed
  expenses <- (asYabList db name $ getExpensesByNameAndDates db name stime etime) !? NotAllowed
  let cs = compareBudgetsBetween (dayToRate (budget^.startDate) stime) (dayToRate (budget^.startDate) etime) budget expenses
  return $ (DL.sortOn (view _1) cs) & traverse . _1 %~ (rateToDay $ budget^.startDate)

listAll :: ListHandler WithBudget
listAll = mkListing jsonO $ \range -> do 
  name <- ask
  db <- (lift . lift) (ask $ view db)
  budget <- (asYabList db name $ getBudgetByName db name) !? NotAllowed
  expenses <- (asYabList db name $ getExpensesByName db name) !? NotAllowed
  let cs = compareBudgetsBetween (offset range) ((offset range) + (count range)) budget expenses
  return $ (DL.sortOn (view _1) cs) & traverse . _1 %~ (rateToDay $ budget^.startDate)
