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
import Data.IxSet
import Data.JSON.Schema hiding (Proxy)
import GHC.Generics hiding (to)
import Rest
import Rest.Dictionary.Types
import Rest.Types.Info
import YabAcid
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Rest.Resource as R

data Identifier = ByDate Day | ByRange

type WithStatus = ReaderT Identifier WithBudget

instance Info Day where
  describe _ = "day"

resource :: Resource WithBudget WithStatus Identifier () Void
resource = mkResourceReader
  { R.name = "status"
  , R.schema = withListing () $ named [ ("on", singleRead ByDate), ("between", single ByRange) ] 
  , R.get = Just get
  , R.list = const list
  }

get :: Handler WithStatus
get = mkHandler (dayRangeParam . jsonO) $ \env -> ask >>= handler env
  where
    handler :: Env () (Maybe DayRange) () -> Identifier -> ExceptT Reason_ WithStatus [BudgetStatusItem]
    handler _ (ByDate date) = do 
      name <- (lift . lift) ask
      db <- (lift . lift . lift) (asks $ view db)
      budget <- (asYabList db name $ getBudgetByName db name) !? NotAllowed
      expenses <- (asYabList db name $ getExpensesByName db name) !? NotAllowed
      let cs = compareBudgetsOn (dayToRate (budget^.startDate) $ UTCTime date 0) budget expenses 
      return $ [cs & _1 %~ (rateToDay $ budget^.startDate)]
    handler (Env _ (Just rnge) _) ByRange = do 
      name <- (lift . lift) ask
      db <- (lift . lift . lift) (asks $ view db)
      budget <- (asYabList db name $ getBudgetByName db name) !? NotAllowed
      expenses <- (asYabList db name $ getExpensesByName db name) !? NotAllowed
      let cs = compareBudgetsBetween (dayToRate (budget^.startDate) (UTCTime (sdate rnge) 0)) (dayToRate (budget^.startDate) (UTCTime (edate rnge) 0)) budget expenses
      return $ (DL.sortOn (view _1) cs) & traverse . _1 %~ (rateToDay $ budget^.startDate)
    handler (Env _ Nothing _) _ = throwE . ParamError $ MissingField "missing fields sdate and edate"

list :: ListHandler WithBudget
list = mkListing jsonO $ \range -> do 
  name <- ask
  db <- (lift . lift) (ask $ view db)
  budget <- (asYabList db name $ getBudgetByName db name) !? NotAllowed
  expenses <- (asYabList db name $ getExpensesByName db name) !? NotAllowed
  let cs = compareBudgetsBetween (offset range) ((offset range) + (count range)) budget expenses
  return $ (DL.sortOn (view _1) cs) & traverse . _1 %~ (rateToDay $ budget^.startDate)
