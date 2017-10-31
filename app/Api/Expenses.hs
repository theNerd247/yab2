{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Expenses (resource) where

import Api.ApiTypes
import Api.ByRange
import Control.Lens hiding ((??),(!?))
import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks, ask)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson
import Data.Budget
import Data.Budget
import Data.Data
import Data.IxSet
import Data.JSON.Schema hiding (Proxy)
import GHC.Generics
import Rest
import Rest.Types.Void
import YabAcid
import qualified Data.List as DL
import qualified Rest.Resource as R

data Identifier = Latest | ByRange

type WithExpense = ReaderT Identifier YabApi

resource :: Resource YabApi WithExpense Identifier Void Void
resource = mkResourceReader
  { R.name = "expenses"
  , R.schema = noListing $ named [("latest", single Latest), ("between", single ByRange)] 
  , R.get = Just get
  }

get :: Handler WithExpense
get = mkHandler (dayRangeParam . jsonO) $ \env -> ask >>= handler env
  where
    handler :: Env () (Maybe DayRange) () -> Identifier -> ExceptT Reason_ WithExpense [ExpenseItem]
    handler (Env _ (Just rnge) _) ByRange = getExpenses (sdate rnge) (edate rnge)
    handler _ ByRange = throwE . ParamError $ MissingField "missing fields sdate and edate"
    handler _ Latest = do 
      now <- liftIO $ getCurrentTime
      let sdate = addDays (-30) $ utctDay now
      let edate = utctDay now
      getExpenses sdate edate
    getExpenses :: Day -> Day -> ExceptT Reason_ WithExpense [ExpenseItem]
    getExpenses s e = do
      db <- (lift . lift) (asks $ view db)
      es <- getExpensesByDate db (dayToDate s) (dayToDate e)
      return $ DL.sortOn (view expenseDate) $ toList es
