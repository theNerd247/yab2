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
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Control.Monad.State (get,put,modify)
import Data.Acid
import Data.BID
import Data.Budget.Expense
import Data.Budget.Internal
import Data.Data
import Data.Default.Time
import Data.Default
import Data.Time
import Data.Traversable (forM)
import Data.Aeson hiding ((.~))
import GHC.Generics hiding (to)
import System.FilePath.Posix
import qualified Data.Csv as CSV
import qualified Data.List as DL
import qualified Data.Text as DT

data Transaction = Transaction
  { _tDate :: Day
  , _tNo :: String
  , _tDesc :: String
  , _tDebit :: Maybe Amount
  , _tCredit :: Maybe Amount
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data Bank = Bank
  { _checking :: Amount
  , _savings :: Amount
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
  -> m FilePath
loadNewTransactionFile bName fp = do 
  e <- loadTransactionFile fp >>= transToExpenses
  let newFP = takeDirectory fp </> (eFP e)
  -- liftIO $ encodeFile newFP e
  liftIO . putStrLn $ "Cannot encode file in loadNewTransactionFile!"
  return newFP
  where
    transToExpenses = mapMOf traversed (toExpense bName)
    eFP e = (show $ startD^.expenseDate) ++ "_" ++ (show $ endD^.expenseDate) ++ ".yaml"
      where
        startD = earliestExpense e
        endD = latestExpense e

toExpense :: (HasTransaction t, MonadIO m) => Name -> t -> m ExpenseItem
toExpense bName t = do 
  bd <- liftIO newBID
  return $ def
    & bid .~ bd
    & name .~ bName
    & expenseDate .~ (t^.tDate)
    & expenseReason .~ (t^.tDesc)
    & amountType .~ case (c > 0) of
      True -> Income
      _ -> Expense ""
    & amount .~ (c - d)
  where
    d = t^.tDebit . to num
    c = t^.tCredit . to num
    num = maybe 0 id
