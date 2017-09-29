{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Budget where

import CSV
import Control.Monad.Catch
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Data.Data
import Data.Foldable
import Data.Default
import Data.Monoid
import Data.Time
import Data.Yaml hiding ((.~))
import GHC.Generics hiding (to)
import System.FilePath.Posix
import qualified Data.Csv as CSV
import qualified Data.Csv as CSV
import qualified Data.List as DL
import qualified Data.Text as DT

type Amount = Double

type Rate = Int

data BudgetType = 
    Income 
  | Expense String
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data BudgetAmount = BudgetAmount
  { _amount :: Amount
  , _budgetType :: BudgetType
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data ExpenseItem = ExpenseItem
  { _date :: Day
  , _reason :: String
  , _expenseItemBudgetAmount :: BudgetAmount
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data BudgetItem = BudgetItem 
  { _rate :: Rate
  , _budgetItemBudgetAmount :: BudgetAmount
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data BudgetStart = BudgetStart
  { _name :: String
  , _startDate :: Day
  , _startAmount :: Amount
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data BudgetList a = BudgetList
  { _budgetListStart :: BudgetStart
  , _items :: [a]
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

type Budget = BudgetList BudgetItem

type Expenses = BudgetList ExpenseItem

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
  , _lastModified :: Rate
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

makeLenses ''Transaction

makeClassy ''BudgetStart

makeClassy ''BudgetItem

makeClassy ''ExpenseItem

makeLenses ''Bank

class HasBudgetAmount a where
  budgetAmount :: Lens' a BudgetAmount

  amount :: Lens' a Amount
  amount = budgetAmount . amount

  budgetType :: Lens' a BudgetType
  budgetType = budgetAmount . budgetType

class HasBudgetList m a | m -> a where
  budgetList :: Lens' m (BudgetList a)

  items :: Lens' m [a]
  items = budgetList . items

  budgetListBudgetStart :: Lens' m BudgetStart
  budgetListBudgetStart = budgetList . budgetListBudgetStart

instance HasBudgetAmount BudgetAmount where
  budgetAmount = id
  
  amount = budgetAmount . (lens gt st)
    where
      st s a = BudgetAmount 
        { _amount = -1*(abs a)
        , _budgetType = _budgetType s
        }
      gt = _amount

  budgetType = budgetAmount . (lens gt st)
    where
      st s t = BudgetAmount 
        { _amount = _amount s
        , _budgetType = t
        }
      gt = _budgetType

instance HasBudgetList (BudgetList a) a where
  budgetList = id
  items = budgetList . go where go f (BudgetList s l) = (\l' -> BudgetList s l') <$> f l
  budgetListBudgetStart = budgetList . go where go f (BudgetList s l) = (\s' -> BudgetList s' l) <$> f s

instance HasBudgetAmount BudgetItem where
  budgetAmount = budgetItemBudgetAmount

instance HasBudgetAmount ExpenseItem where
  budgetAmount = expenseItemBudgetAmount

instance HasBudgetStart (BudgetList a) where
  budgetStart = budgetListBudgetStart

instance CSV.FromRecord Transaction

instance CSV.ToRecord Transaction

instance Default BudgetType where
  def = Income

instance Default Day where
  def = fromGregorian 0 0 0

instance Default BudgetStart

instance (Default a) => Default (BudgetList a)

instance Default BudgetAmount

instance Default ExpenseItem 

instance Default BudgetItem 

instance Default Bank 

instance FromJSON BudgetType where
  parseJSON (String s)
    | s == "income" = return Income
    | otherwise = return $ Expense (DT.unpack s)
  parseJSON _ = fail "Budget type is the wrong yaml type - should be a string"

instance FromJSON BudgetAmount

instance FromJSON BudgetStart

instance FromJSON ExpenseItem

instance FromJSON BudgetItem

instance FromJSON Bank

instance ToJSON ExpenseItem

instance ToJSON BudgetType where
  toJSON Income = String $ DT.pack "income"
  toJSON (Expense s) = String $ DT.pack s

instance ToJSON BudgetItem

instance ToJSON Bank

instance ToJSON BudgetAmount

instance ToJSON BudgetStart
