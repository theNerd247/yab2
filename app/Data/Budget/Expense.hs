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
import Data.Audit
import Data.Acid
import Data.BID
import Data.Budget.Internal
import Data.Data
import Data.DayDefault
import Data.Default
import Data.IxSet
import Data.SafeCopy
import Data.Time
import GHC.Generics hiding (to)
import Data.Yaml hiding ((.~))
import qualified Data.Text as DT
import qualified Data.Text as DT

data ExpenseItem = ExpenseItem
  { _expenseDate :: Day
  , _expenseReason :: String
  , _expenseItemBudgetAmount :: BudgetAmount
  , _expenseBID :: BID
  , _expenseBudgetName :: Name
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

type ExpenseList = YabList ExpenseItem

type ExpenseDB = YabDB ExpenseItem

type ExpenseAudit = Audit ExpenseItem

type ExpenseAuditDB = AuditDB ExpenseItem

makeClassy ''ExpenseItem

$(deriveSafeCopy 0 'base ''ExpenseItem)

instance HasBID ExpenseItem where
  bid = expenseBID

instance BudgetAtPeriod ExpenseItem where
  budgetAmountAtPeriod s p = to gt
    where 
      gt e
        | dayToRate (s^.startDate) (e^.expenseDate) == p = e^.amount
        | otherwise = 0

instance Default ExpenseItem

instance FromJSON ExpenseItem where
  parseJSON v@(Object o) = ExpenseItem 
    <$> o .: "date" 
    <*> o .: "reason"
    <*> (parseJSON v)
    <*> o .: "id"
    <*> o .: "name"
  parseJSON _ = mempty

instance HasBudgetAmount ExpenseItem where
  budgetAmount = expenseItemBudgetAmount

instance HasName ExpenseItem where
  name = expenseBudgetName

instance Indexable ExpenseItem where
  empty = ixSet
    [ ixFun $ (:[]) . (view bid)
    , ixFun $ (:[]) . (view name)
    , ixFun $ (:[]) . (view expenseDate)
    , ixFun $ (:[]) . (view amountType)
    , ixFun $ (:[]) . (view amount)
    ]

instance ToJSON ExpenseItem where
  toJSON ei = object $
    budgetAmountJSON (ei^.budgetAmount)
    ++ ["reason" .= (ei^.expenseReason)
       ,"date" .= (ei^.expenseDate)
       , "id" .= (ei^.bid)
       , "name" .= (ei^.name)
       ]

{-queryExpenses :: Query ExpensesDB Expenses-}
{-queryExpenses = asks $ view expensesDB-}

{-insertExpenses :: [ExpenseItem] -> Update ExpensesDB ()-}
{-insertExpenses es = expensesDB %= union (fromList es)-}

{-insertExpense :: ExpenseItem -> Update ExpensesDB ()-}
{-insertExpense e = expensesDB %= union-}

upsertExpenses :: [ExpenseItem] -> ExpenseDB -> (ExpenseDB,[[ExpenseItem]])
upsertExpenses es db = foldr upsertE (db,[]) es
  where
    upsertE e (db,dups) = either (\x -> (x,[])) (\x -> (db,x:dups)) $ upsertExpense e db

upsertExpense :: ExpenseItem -> ExpenseDB -> Either ExpenseDB [ExpenseItem]
upsertExpense e db = upsertExpense' e db (expenseDuplicates e db)
  where
    upsertExpense' e db [] = Left $ insert e db
    upsertExpense' e _ dups = Right $ e : dups

expenseDuplicates :: ExpenseItem -> ExpenseDB -> [ExpenseItem]
expenseDuplicates e db = toList $ db @= (e^.expenseDate) @= (e^.amount)

{-updateAt :: (a -> Bool) -> a -> [a] -> [a]-}
{-updateAt p x xs = maybe xs (\i -> xs & element i .~ x) (DL.findIndex p xs)-}

{-mergeExpenses :: Expenses -> Expenses -> (Expenses, [ExpenseItem])-}
{-mergeExpenses xs ys = (xs & items .~ merged^._1, merged^._2)-}
  {-where-}
    {-merged = mergeDups isDuplicate (xs^.items) (ys^.items)-}
    {-isDuplicate a b = (a^.expenseDate == b^.expenseDate) && (a^.amount == b^.amount)-}

{-mergeDups :: (a -> a -> Bool) -> [a] -> [a] -> ([a],[a])-}
{-mergeDups pred as bs = foldr merged (bs,[]) as-}
  {-where-}
    {-merged x kept-}
      {-| any (pred x) (kept^._1) = kept & _2 %~ (x:)-}
      {-| otherwise = kept & _1 %~ (x:)-}
