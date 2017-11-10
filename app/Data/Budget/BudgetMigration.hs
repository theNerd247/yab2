{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Budget.BudgetMigration where

import Data.SafeCopy
import Control.Lens
import Data.Data
import GHC.Generics
import Data.BIDMigration
import Data.Budget.Internal
import Data.BID
import Data.Budget.InternalMigration
import Data.Time (UTCTime)

data BudgetItem_v0 = BudgetItem_v0
  { _budgetItemRate :: Rate_v0
  , _budgetItemBudgetAmount :: BudgetAmount
  , _budgetName :: Name
  , _budgetItemBID :: BID
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

$(deriveSafeCopy 0 'base ''BudgetItem_v0)
