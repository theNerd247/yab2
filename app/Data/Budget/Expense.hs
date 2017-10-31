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
import Data.Budget.Internal
import Data.Data
import Data.Default
import Data.Default.IxSet
import Data.Default.Time
import Data.IxSet
import Data.JSON.Schema hiding (Proxy, Object)
import Data.SafeCopy
import Data.Time
import GHC.Generics hiding (to)
import qualified Data.List as DL
import qualified Data.Text as DT

data ExpenseItem = ExpenseItem
  { _expenseDate :: UTCTime
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

instance JSONSchema ExpenseItem where
  schema = gSchema

instance ToJSON ExpenseItem where
  toJSON ei = object $
    budgetAmountJSON (ei^.budgetAmount)
    ++ ["reason" .= (ei^.expenseReason)
       ,"date" .= (ei^.expenseDate)
       , "id" .= (ei^.bid)
       , "name" .= (ei^.name)
       ]

updateAt :: (a -> Bool) -> a -> [a] -> [a]
updateAt p x xs = maybe (x:xs) (\i -> xs & element i .~ x) (DL.findIndex p xs)

-- returns a db of items that are ok to insert into the passed db
-- without giving duplicates and the list of duplicate items
--
upsertExpenses :: [ExpenseItem] -> ExpenseDB -> (ExpenseDB,[[ExpenseItem]])
upsertExpenses es db = foldr upsertE (def,[]) es
  where
    upsertE e (mdb,dups) = either (\x -> (insert x mdb,[])) (\x -> (mdb,updateAt (compareDups (head x)) x dups)) $ upsertExpense e db
    compareDups e [] = False
    compareDups e (x:xs) = (e^.expenseDate) == (x^.expenseDate) && (e^.amount) == (x^.amount)

upsertExpense :: ExpenseItem -> ExpenseDB -> Either ExpenseItem [ExpenseItem]
upsertExpense e db = upsertExpense' e (expenseDuplicates e db)
  where
    upsertExpense' e [] = Left $ e
    upsertExpense' e dups = Right $ e : dups

expenseDuplicates :: ExpenseItem -> ExpenseDB -> [ExpenseItem]
expenseDuplicates e db = toList $ db @= (e^.expenseDate) @= (e^.amount)

earliestExpense :: [ExpenseItem] -> ExpenseItem
earliestExpense ts = DL.minimumBy (\a b -> compare (a^.expenseDate) (b^.expenseDate)) ts

latestExpense :: [ExpenseItem] -> ExpenseItem
latestExpense ts = DL.maximumBy (\a b -> compare (a^.expenseDate) (b^.expenseDate)) ts
