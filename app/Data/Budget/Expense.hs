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

module Data.Budget.Expense where

import Control.Lens hiding ((.=),Indexable)
import Data.Acid
import Data.Aeson hiding ((.~))
import Data.Audit
import Data.BID
import Data.Budget.Rate
import Data.Budget.Name
import Data.Budget.Amount
import Data.Budget.YabList
import Data.Budget.Items
import Data.Data
import Data.Default
import Data.Default.IxSet
import Data.Default.Time
import Data.IxSet
import Data.SafeCopy
import Data.Time
import GHC.Generics hiding (to)
import qualified Data.List as DL
import qualified Data.Text as DT

data ExpenseItem = ExpenseItem
  { _expenseRate :: Rate
  , _expenseReason :: String
  , _expenseItemAmount :: Amount
  , _expenseBID :: BID
  , _expenseBudgetName :: Name
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

makeClassy ''ExpenseItem

instance HasBID ExpenseItem where
  bid = expenseBID

instance Default ExpenseItem

instance FromJSON ExpenseItem where
  parseJSON v@(Object o) = ExpenseItem 
    <$> o .: "date" 
    <*> o .: "reason"
    <*> (parseJSON v)
    <*> o .: "id"
    <*> o .: "name"
  parseJSON _ = mempty

instance HasAmount ExpenseItem where
  amount = expenseItemAmount

instance HasName ExpenseItem where
  name = expenseBudgetName

instance HasRate ExpenseItem where
  rate = expenseRate

instance Indexable ExpenseItem where
  empty = ixSet
    [ ixFun $ (:[]) . (view bid)
    , ixFun $ (:[]) . (view name)
    , ixFun $ (:[]) . (view expenseRate)
    , ixFun $ (:[]) . (view amount)
    ]

{-data ExpenseItem_v0 = ExpenseItem_v0-}
  {-{ _expenseRateV0 :: UTCTime-}
  {-, _expenseReasonV0 :: String-}
  {-, _expenseItemAmountV0 :: BudgetAmount_v0-}
  {-, _expenseBIDV0 :: BID-}
  {-, _expenseBudgetNameV0 :: Name-}
  {-} deriving (Eq,Ord,Show,Read)-}

{-$(deriveSafeCopy 0 'base ''ExpenseItem_v0)-}

data ExpenseItem_v1 = ExpenseItem_v1
  { _expenseRateV1 :: Rate
  , _expenseReasonV1 :: String
  , _expenseItemBudgetAmountV1 :: BudgetAmount_v0
  , _expenseBIDV1 :: BID
  , _expenseBudgetNameV1 :: Name
  } deriving (Eq,Ord,Show,Read)

{-instance Migrate ExpenseItem_v1 where-}
  {-type MigrateFrom ExpenseItem_v1 = ExpenseItem_v0-}
  {-migrate e = ExpenseItem_v1-}
    {-{ _expenseRateV1 = OneTime $ _expenseRateV0 e-}
    {-, _expenseReasonV1 = _expenseReasonV0 e-}
    {-, _expenseItemBudgetAmountV1 = _expenseItemAmountV0 e-}
    {-, _expenseBIDV1 = _expenseBIDV0 e-}
    {-, _expenseBudgetNameV1 = _expenseBudgetNameV0 e-}
    {-}-}

$(deriveSafeCopy 0 'base ''ExpenseItem_v1)

instance Migrate ExpenseItem where
  type MigrateFrom ExpenseItem = ExpenseItem_v1
  migrate e = ExpenseItem
    { _expenseRate = _expenseRateV1 e
    , _expenseReason = _expenseReasonV1 e
    , _expenseItemAmount = _budgetAmountAmountV0 $ _expenseItemBudgetAmountV1 e
    , _expenseBID = _expenseBIDV1 e
    , _expenseBudgetName = _expenseBudgetNameV1 e
    }

$(deriveSafeCopy 1 'extension ''ExpenseItem)

instance ToJSON ExpenseItem where
  toJSON ei = object $
    [ "amount" .= (ei^.amount)
    , "reason" .= (ei^.expenseReason)
    , "rate" .= (ei^.expenseRate)
    , "id" .= (ei^.bid)
    , "name" .= (ei^.name)
    ]

updateAt :: (a -> Bool) -> a -> [a] -> [a]
updateAt p x xs = maybe (x:xs) (\i -> xs & element i .~ x) (DL.findIndex p xs)

-- returns a db of items that are ok to insert into the passed db
-- without giving duplicates and the list of duplicate items
--
upsertExpenses :: (Foldable t) => t ExpenseItem -> IxSet ExpenseItem -> (IxSet ExpenseItem,[[ExpenseItem]])
upsertExpenses es db = foldr upsertE (def,[]) es
  where
    upsertE e (mdb,dups) = either (\x -> (insert x mdb,[])) (\x -> (mdb,updateAt (compareDups (head x)) x dups)) $ upsertExpense e db
    compareDups e [] = False
    compareDups e (x:xs) = (e^.expenseRate) == (x^.expenseRate) && (e^.amount) == (x^.amount)

{-upsertExpense :: ExpenseItem -> ExpenseDB -> Either ExpenseItem [ExpenseItem]-}
upsertExpense e db = upsertExpense' e (expenseDuplicates e $ db @= (e^.name))
  where
    upsertExpense' e [] = Left $ e
    upsertExpense' e dups = Right $ e : dups

{-expenseDuplicates :: ExpenseItem -> ExpenseDB -> [ExpenseItem]-}
expenseDuplicates e db = toList $ db @= (e^.expenseRate) @= (e^.amount)
