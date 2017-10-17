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
import Control.Lens hiding ((.=))
import Control.Monad.Reader (ask)
import Control.Monad.State (get,put,modify)
import Control.Monad.Catch
import Data.Acid
import Data.DayDefault
import Data.Data
import Data.Default
import Data.Budget.Expense
import Data.Time
import Data.Traversable (forM)
import Data.Yaml hiding ((.~))
import GHC.Generics hiding (to)
import System.FilePath.Posix
import Control.Monad.IO.Class
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

instance Default Bank 

instance Default Transaction

instance FromJSON Bank

instance ToJSON Bank

loadYamlFile :: (FromJSON a) => FilePath -> IO a
loadYamlFile fp = decodeFileEither fp >>= either throwIO return

loadTransactionFile :: FilePath -> IO [Transaction]
loadTransactionFile = loadCSVFile

loadNewTransactionFile :: (MonadIO m)
  String  -- ^ name of budget to load the transactions to
  -> FilePath -- ^ file path to the transactions
  -> m FilePath
loadNewTransactionFile bName fp = do 
  e <- loadTransactionFile fp >>= transToExpenses
  let newFP = takeDirectory fp </> (eFP e)
  encodeFile newFP e
  return newFP
  where
    transToExpenses ts = do 
      newEs <- mapMOf items toExpense ts
      return $ def & name .~ bname & items .~ newEs
    eFP e = (show $ startD^.expenseDate) ++ "_" ++ (show $ endD^.expenseDate) ++ ".yaml"
      where
        startD = DL.minimumBy (\a b -> compare (a^.expenseDate) (b^.expenseDate)) $ e^.items
        endD = DL.maximumBy (\a b -> compare (a^.expenseDate)  (b^.expenseDate)) $ e^.items

toExpense :: (HasTransaction t, MonadIO m) => t -> m ExpenseItem
toExpense t = do 
  bd <- newBID
  return $ def
    & bid .~ bd
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
