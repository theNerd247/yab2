{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Bank where

import CSV
import Control.Lens
import Data.Aeson
import Control.Monad.IO.Class
import Data.BID
import Data.Budget.Amount
import Data.Budget.Expense
import Data.Budget.Name
import Data.Budget.Rate
import Data.Data
import Data.Default
import Data.Default.Time
import Data.Time
import GHC.Generics hiding (to, from)
import System.FilePath.Posix
import qualified Data.Csv as CSV
import qualified Data.List as DL
import qualified Data.Text as DT

data Transaction = Transaction
  { _tDate :: Day
  , _tNo :: String
  , _tDesc :: String
  , _tDebit :: Maybe Double
  , _tCredit :: Maybe Double
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data Bank = Bank
  { _checking :: Double
  , _savings :: Double
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

makeClassy ''Bank

makeClassy ''Transaction

instance CSV.FromRecord Transaction

instance Default Bank 

instance Default Transaction

instance FromJSON Bank

instance ToJSON Bank

loadTransactionFile :: (MonadIO m) => FilePath -> m [Transaction]
loadTransactionFile = liftIO . loadCSVFile

loadNewTransactionFile :: (MonadIO m) =>
  Name  -- ^ name of budget to load the transactions to
  -> FilePath -- ^ file path to the transactions
  -> m [ExpenseItem]
loadNewTransactionFile bName fp = 
  loadTransactionFile fp >>= transToExpenses
  where
    transToExpenses = mapMOf traversed (toExpense bName)

toExpense :: (HasTransaction t, MonadIO m) => Name -> t -> m ExpenseItem
toExpense bName t = do 
  bd <- liftIO newBID
  return $ def
    & bid .~ bd
    & name .~ bName
    & expenseRate .~ (OneTime $ t^.tDate.dayDateIso)
    & expenseReason .~ (t^.tDesc)
    & amount .~ (c - d)
  where
    d = t^.tDebit . to num . from amountDoubleIso
    c = t^.tCredit . to num . from amountDoubleIso
    num = maybe 0 id
