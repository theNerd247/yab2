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
import Data.IxSet hiding (getRange)
import Data.JSON.Schema hiding (Proxy)
import GHC.Generics
import Rest
import Rest.Types.Void
import Rest.Handler
import YabAcid
import qualified Data.List as DL
import qualified Rest.Resource as R

type Identifier = ()

data MID = Latest | ByRange

type WithExpense = ReaderT Identifier YabApi

resource :: Resource YabApi WithExpense Identifier MID Void
resource = mkResourceReader
  { R.name = "expenses"
  , R.schema = withListing Latest $ named [("between", listing ByRange)] 
  , R.list = list
  }

list :: MID -> ListHandler YabApi
list Latest = getLatest
list ByRange = getRange

getLatest :: ListHandler YabApi
getLatest = mkCustomListing jsonO $ \env -> do
  db <- lift (asks $ view db)
  edb <- getExpensesDB db
  return . take 30 . DL.sortOn (view expenseDate) $ toList edb

getRange :: ListHandler YabApi
getRange = mkCustomListing (dayRangeParam . jsonO) $ \env -> 
  getExpensesByRange (sdate $ param env) (edate $ param env)

getExpensesByRange :: Day -> Day -> ExceptT Reason_ YabApi [ExpenseItem]
getExpensesByRange s e = do
  db <- lift (asks $ view db)
  es <- getExpensesByDate db (dayToDate s) (dayToDate e)
  return $ DL.sortOn (view expenseDate) $ toList es
