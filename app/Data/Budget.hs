{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Budget where

import CSV
import Control.Monad.Catch
import Control.Lens hiding ((.=))
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
  { _expenseDate :: Day
  , _expenseReason :: String
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

class (HasBudgetAmount a) => HasBudgetList m a | m -> a where
  budgetList :: Lens' m (BudgetList a)

  items :: Lens' m [a]
  items = budgetList . items

  budgetListBudgetStart :: Lens' m BudgetStart
  budgetListBudgetStart = budgetList . budgetListBudgetStart

class (HasBudgetAmount a) => BudgetAtPeriod a where
  budgetAtPeriod :: (HasBudgetStart s) => s -> Rate -> Getter a Amount

instance BudgetAtPeriod BudgetItem where
  budgetAtPeriod _ p = to gt
    where
      gt b = (b^.amount)*(fromIntegral . floor . toRational $ p `div` (b^.rate))

instance BudgetAtPeriod ExpenseItem where
  budgetAtPeriod s p = to gt
    where 
      gt e
        | dayToRate (s^.startDate) (e^.expenseDate) == p = e^.amount
        | otherwise = 0

instance HasBudgetAmount BudgetAmount where
  budgetAmount = id
  
  amount = budgetAmount . (lens gt st)
    where
      st s a = BudgetAmount 
        { _amount = toAmnt (_budgetType s)
        , _budgetType = _budgetType s
        }
        where
        toAmnt Income = abs a
        toAmnt _ = -1*(abs a)
      gt = _amount

  budgetType = budgetAmount . (lens gt st)
    where
      st s t = BudgetAmount 
        { _amount = _amount s
        , _budgetType = t
        }
      gt = _budgetType

instance (HasBudgetAmount a) => HasBudgetList (BudgetList a) a where
  budgetList = id
  items = budgetList . go where go f (BudgetList s l) = (\l' -> BudgetList s l') <$> f l
  budgetListBudgetStart = budgetList . go where go f (BudgetList s l) = (\s' -> BudgetList s' l) <$> f s

instance HasBudgetAmount BudgetItem where
  budgetAmount = budgetItemBudgetAmount

instance HasBudgetAmount ExpenseItem where
  budgetAmount = expenseItemBudgetAmount

instance (HasBudgetAmount a) => HasBudgetStart (BudgetList a) where
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

instance FromJSON Bank

instance FromJSON BudgetAmount where
  parseJSON (Object o) = BudgetAmount
    <$> o .: "amount"
    <*> o .: "type"
  parseJSON _ = mempty

instance (FromJSON a) => FromJSON (BudgetList a) where
  parseJSON v@(Object o) = BudgetList
    <$> (parseJSON v)
    <*> o .: "items"
  parseJSON _ = mempty

instance FromJSON BudgetItem where
  parseJSON v@(Object o) = BudgetItem 
    <$> o .: "rate" 
    <*> (parseJSON v)
  parseJSON _ = mempty

instance FromJSON BudgetStart where
  parseJSON (Object o) = BudgetStart
    <$> o .: "name"
    <*> o .: "start-date"
    <*> o .: "start-amount"
  parseJSON _ = mempty

instance FromJSON BudgetType where
  parseJSON (String s)
    | s == "income" = return Income
    | otherwise = return $ Expense (DT.unpack s)
  parseJSON _ = mempty

instance FromJSON ExpenseItem where
  parseJSON v@(Object o) = ExpenseItem 
    <$> o .: "date" 
    <*> o .: "reason"
    <*> (parseJSON v)
  parseJSON _ = mempty

instance ToJSON Bank

instance ToJSON BudgetAmount where
  toJSON = object . budgetAmountJSON

budgetAmountJSON a =
    ["amount" .= (a^.amount)
    ,"type" .= (a^.budgetType)
    ]

instance (ToJSON a, HasBudgetAmount a) => ToJSON (BudgetList a) where 
  toJSON b = object $
    budgetStartJSON (b^.budgetListBudgetStart)
    ++ ["items" .= toJSON (b^.items)]

instance ToJSON BudgetItem where
  toJSON bi = object $ 
    budgetAmountJSON (bi^.budgetItemBudgetAmount)
    ++ ["rate" .= (bi^.rate)]

instance ToJSON BudgetStart where
  toJSON = object . budgetStartJSON

budgetStartJSON b =
    ["name" .= (b^.name)
    ,"start-date" .= (b^.startDate)
    ,"start-amount" .= (b^.startAmount)
    ]

instance ToJSON BudgetType where
  toJSON Income = String $ DT.pack "income"
  toJSON (Expense s) = String $ DT.pack s


instance ToJSON ExpenseItem where
  toJSON ei = object $
    budgetAmountJSON (ei^.expenseItemBudgetAmount)
    ++ ["reason" .= (ei^.expenseReason)
       ,"date" .= (ei^.expenseDate)
       ]

dayToRate :: Day -> Day -> Rate
dayToRate s = fromInteger . flip diffDays s

rateToDay :: Day -> Rate -> Day
rateToDay s = flip addDays s . toInteger

-- runs a budget for "p" periods given a starting amount "start" and returns the
-- final balance. 
getBalanceAtPeriod :: (BudgetAtPeriod a, HasBudgetStart (f a), HasBudgetList (f a) a) => Rate -> (f a) -> Amount
getBalanceAtPeriod p b = (b^.startAmount) + (sum $ b^.items^..traverse.budgetAtPeriod (b^.budgetStart) p)

-- gets the budget balance from start to end periods (both inclusive)
getBalancesBetween :: (BudgetAtPeriod a, HasBudgetStart (f a), HasBudgetList (f a) a) => Rate -> Rate -> (f a) -> [Amount]
getBalancesBetween start end b = [start..end]^..traverse . to (flip getBalanceAtPeriod b)

compareBudgetsBetween :: (BudgetAtPeriod a, HasBudgetStart (f a), HasBudgetList (f a) a, BudgetAtPeriod b, HasBudgetStart (g b), HasBudgetList (g b) b) => Rate -> Rate -> (f a) -> (g b) -> [Amount]
compareBudgetsBetween start end b1 b2 = [start..end]^..traverse . to compare
  where
    compare p = (getBalanceAtPeriod p b2) - (getBalanceAtPeriod p b1)

getEmptyDate :: (BudgetAtPeriod a, HasBudgetStart (f a), HasBudgetList (f a) a) => f a -> Rate
getEmptyDate budget = g 0
    where
      g n
        | (f n) <= 0 = n
        | otherwise = g (n+1)
      f n = getBalanceAtPeriod (n+1) budget

printBalances :: (BudgetAtPeriod a, HasBudgetStart (f a), HasBudgetList (f a) a) => Rate -> Rate -> f a -> IO ()
printBalances start end budget = sequence_ $ [start..end]^..traverse . to printBal
  where
    printBal d = putStrLn $ 
         (show d)
      ++ ": "
      ++ (show $ getBalanceAtPeriod d budget)

currentBudgetBal :: (BudgetAtPeriod a, HasBudgetStart (f a), HasBudgetList (f a) a) => f a -> IO Amount
currentBudgetBal b = do 
  n <- utctDay <$> getCurrentTime 
  return $ getBalanceAtPeriod (dayToRate (b^.startDate) n) b
