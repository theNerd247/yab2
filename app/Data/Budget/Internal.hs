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
{-# LANGUAGE UndecidableInstances #-}

module Data.Budget.Internal where

import Control.Lens hiding ((.=),Indexable)
import Control.Monad.Reader (asks)
import Control.Monad.State (get,put,modify)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Acid
import Data.Data
import Data.Default
import Data.IxSet
import Data.SafeCopy
import Data.Monoid
import Data.DayDefault
import Data.Time
import Data.Yaml hiding ((.~))
import GHC.Generics hiding (to)
import qualified Data.Text as DT
import qualified Data.List as DL

type Amount = Double

type Rate = Int

type Name = String

type YabDB = IxSet 

data AmountType = 
    Income 
  | Expense String
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data BudgetAmount = BudgetAmount
  { _amount :: Amount
  , _amountType :: AmountType
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data StartInfo = StartInfo
  { _startInfoBName :: Name
  , _startDate :: Day
  , _startAmount :: Amount
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data YabList a = YabList
  { _yabListStartInfo :: StartInfo
  , _items :: [a]
  } deriving (Eq,Ord, Show,Read,Data,Typeable,Generic)

makeClassy ''StartInfo

$(deriveSafeCopy 0 'base ''AmountType)

$(deriveSafeCopy 0 'base ''BudgetAmount)

$(deriveSafeCopy 0 'base ''StartInfo)

$(deriveSafeCopy 0 'base ''YabList)

class HasName m where
  name :: Lens' m Name

class HasBudgetAmount a where
  budgetAmount :: Lens' a BudgetAmount

  amount :: Lens' a Amount
  amount = budgetAmount . amount

  amountType :: Lens' a AmountType
  amountType = budgetAmount . amountType

class HasYabDB m a | m -> a where
  yabDB :: Lens' m (YabDB a)

class HasYabList m a | m -> a where
  yabList :: Lens' m (YabList a)

  items :: Lens' m [a]
  items = yabList . items

  yabListStartInfo :: Lens' m StartInfo
  yabListStartInfo = yabList . yabListStartInfo

class BudgetAtPeriod a where
  budgetAmountAtPeriod :: (HasStartInfo s) => s -> Rate -> Getter a Amount

instance Default BudgetAmount

instance Default AmountType where
  def = Income

instance Default StartInfo

instance (Default a) => Default (YabList a) 

instance FromJSON AmountType where
  parseJSON (String s)
    | s == "income" = return Income
    | otherwise = return $ Expense (DT.unpack s)
  parseJSON _ = mempty

instance FromJSON BudgetAmount where
  parseJSON (Object o) = BudgetAmount
    <$> o .: "amount"
    <*> o .: "type"
  parseJSON _ = mempty

instance (FromJSON a) => FromJSON (YabList a) where
  parseJSON v@(Object o) = YabList
    <$> (parseJSON v)
    <*> o .: "items"
  parseJSON _ = mempty

instance FromJSON StartInfo where
  parseJSON (Object o) = StartInfo
    <$> o .: "name"
    <*> o .: "start-date"
    <*> o .: "start-amount"
  parseJSON _ = mempty

instance Functor YabList where
  fmap f l = YabList
    { _yabListStartInfo = _yabListStartInfo l
    , _items = fmap f (_items l)
    }

instance HasBudgetAmount BudgetAmount where
  budgetAmount = id
  
  amount = budgetAmount . (lens gt st)
    where
      st s a = BudgetAmount 
        { _amount = toAmnt (_amountType s)
        , _amountType = _amountType s
        }
        where
        toAmnt Income = abs a
        toAmnt _ = -1*(abs a)
      gt = _amount

  amountType = budgetAmount . (lens gt st)
    where
      st s t = BudgetAmount 
        { _amount = _amount s
        , _amountType = t
        }
      gt = _amountType

instance HasName StartInfo where
  name = startInfoBName

instance HasYabDB (YabDB a) a where
  yabDB = id

instance HasYabList (YabList a) a where
  yabList = id
  items = yabList . go where go f (YabList s l) = (\l' -> YabList s l') <$> f l
  yabListStartInfo = yabList . go where go f (YabList s l) = (\s' -> YabList s' l) <$> f s

instance HasStartInfo (YabList a) where
  startInfo = yabListStartInfo

instance ToJSON BudgetAmount where
  toJSON = object . budgetAmountJSON

instance ToJSON AmountType where
  toJSON Income = String $ DT.pack "income"
  toJSON (Expense s) = String $ DT.pack s

instance ToJSON StartInfo where
  toJSON = object . budgetStartJSON

instance (ToJSON a, HasBudgetAmount a) => ToJSON (YabList a) where 
  toJSON b = object $
    budgetStartJSON (b^.yabListStartInfo)
    ++ ["items" .= toJSON (b^.items)]

budgetAmountJSON a =
    ["amount" .= (a^.amount)
    ,"type" .= (a^.amountType)
    ]

budgetStartJSON b =
    ["name" .= (b^.name)
    ,"start-date" .= (b^.startDate)
    ,"start-amount" .= (b^.startAmount)
    ]

dayToRate :: Day -> Day -> Rate
dayToRate s = fromInteger . flip diffDays s

rateToDay :: Day -> Rate -> Day
rateToDay s = flip addDays s . toInteger

-- runs a budget for "p" periods given a starting amount "start" and returns the
-- final balance. 
{-getBalanceAtPeriod :: (BudgetAtPeriod a, HasStartInfo (f a), Traversal' (f a) a) => Rate -> (f a) -> Amount-}
getBalanceAtPeriod p b = (b^.startAmount) + (sum $ b^.items^..traverse.budgetAmountAtPeriod (b^.startInfo) p)

-- gets the budget balance from start to end periods (both inclusive)
{-getBalancesBetween :: (BudgetAtPeriod a, HasStartInfo (f a), HasYabList (f a) a) => Rate -> Rate -> (f a) -> [Amount]-}
getBalancesBetween start end b = [start..end]^..traverse . to (flip getBalanceAtPeriod b)

-- returns the difference of the budget balance (b2 - b1) at each period between
-- the start and end times
compareBudgetsBetween :: (BudgetAtPeriod a, HasStartInfo (f a), HasYabList (f a) a, BudgetAtPeriod b, HasStartInfo (g b), HasYabList (g b) b) => Rate -> Rate -> (f a) -> (g b) -> [Amount]
compareBudgetsBetween start end b1 b2 = [start..end]^..traverse . to compare
  where
    compare p = (getBalanceAtPeriod p b2) - (getBalanceAtPeriod p b1)

-- get's the first period where the budget balance is <= 0
getEmptyDate :: (BudgetAtPeriod a, HasStartInfo (f a), HasYabList (f a) a) => f a -> Rate
getEmptyDate budget = g 0
    where
      g n
        | (f n) <= 0 = n
        | otherwise = g (n+1)
      f n = getBalanceAtPeriod (n+1) budget

-- prints the balance of the budget at each period from start to end
printBalances :: (BudgetAtPeriod a, HasStartInfo (f a), HasYabList (f a) a) => Rate -> Rate -> f a -> IO ()
printBalances start end budget = sequence_ $ [start..end]^..traverse . to printBal
  where
    printBal d = putStrLn $ 
         (show d)
      ++ ": "
      ++ (show $ getBalanceAtPeriod d budget)

-- gets the budget balance for the current day (this uses utc time)
currentBudgetBal :: (BudgetAtPeriod a, HasStartInfo (f a), HasYabList (f a) a) => f a -> IO Amount
currentBudgetBal b = do 
  n <- utctDay <$> getCurrentTime
  return $ getBalanceAtPeriod (dayToRate (b^.startDate) n) b

queryYabDB :: (HasYabDB s a, MonadReader s m) => m (YabDB a)
queryYabDB = asks $ view yabDB

updateYabDB :: (HasYabDB s a, MonadState s m) => (YabDB a) -> m (YabDB a)
updateYabDB db = assign yabDB db >> return db

queryDBItems :: (HasYabDB s a, MonadReader s m) => (YabDB a -> YabDB a) -> m (YabDB a)
queryDBItems f = asks $ view (yabDB.to f)

-- updates each element by the given key using updateIx
updateDBItems :: (HasYabDB s a, MonadState s m, Ord a, Indexable a, Typeable k, Typeable a) => k -> [a] -> m ()
updateDBItems key values = yabDB %= (appEndo (foldMap (Endo . updateIx key) values))

queryYabList :: (HasYabList s a, MonadReader s m) => m (YabList a)
queryYabList = asks $ view yabList

updateYabList ::(HasYabList s a, MonadState s m) => (YabList a) -> m (YabList a)
updateYabList l = assign yabList l >> return l

-- runs a query that preserves the index of the elements found
queryListItems :: (HasYabList s a, MonadReader s m) =>  (a -> Bool) -> m (YabList (Int,a))
queryListItems pred = do 
  db <- asks $ view yabList
  return $ YabList 
    { _yabListStartInfo = db^.startInfo
    , _items = itoListOf (items.folded.filtered pred) db
    }

-- updates each element by the given key using updateIx
updateListItems :: (HasYabList s a, MonadState s m) => [(Int,a)] -> m (YabList a)
updateListItems values = do 
  yabList.items %= (appEndo (foldMap (Endo . upsert) values))
  use yabList
  where
    upsert (i,x) xs
      | i < 0 = xs
      | otherwise = xs & element i .~ x