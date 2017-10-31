{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Api.Budget.Status (resource) where

import Api.Budget (WithBudget)
import Control.Error.Util ((??),(!?))
import Control.Lens hiding ((??),(!?))
import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks, ask)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson
import Data.Budget
import Data.Data
import Rest.Dictionary.Types
import Data.IxSet
import Data.JSON.Schema hiding (Proxy)
import Data.JSON.Schema.Combinators (value)
import GHC.Generics hiding (to)
import Rest.Types.Info
import YabAcid
import qualified Data.Text as T
import qualified Data.List as DL

import Api.ApiTypes
import Rest
import Rest.Types.Void
import qualified Rest.Resource as R

data Identifier = ByDate Day | ByRange

data DayRange = DayRange
  { sdate :: Day
  , edate :: Day
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

instance ToJSON DayRange
instance FromJSON DayRange

instance JSONSchema Day where
  schema _ = value

instance JSONSchema DayRange where
  schema = gSchema

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

dayRangeParam = mkPar $ Param ["sdate","edate"] parse
  where
    parse [Nothing, Nothing] = Right Nothing
    parse [Just _, Nothing] = Left $ MissingField "edate"
    parse [Nothing, Just _] = Left $ MissingField "sdate"
    parse [Just s, Just e] = Right . Just $ DayRange (read s) (read e)

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
