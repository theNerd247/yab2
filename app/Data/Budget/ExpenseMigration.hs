{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Budget.ExpenseMigration where

import Data.SafeCopy
import Control.Lens
import Data.Data
import GHC.Generics
import Data.BIDMigration
import Data.BID
import Data.Budget.Internal
import Data.Budget.InternalMigration
import Data.Time (UTCTime)

data ExpenseItem_v0 = ExpenseItem_v0
  { _expenseDate :: UTCTime
  , _expenseReason :: String
  , _expenseItemBudgetAmount :: BudgetAmount
  , _expenseBID :: BID
  , _expenseBudgetName :: Name
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

$(deriveSafeCopy 0 'base ''ExpenseItem_v0)
