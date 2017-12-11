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

module Data.Budget.Budget where

import Control.Lens hiding ((.=),Indexable)
import Data.Acid
import Data.Aeson hiding ((.~))
import Data.Audit
import Data.BID
import Data.Budget.Internal
import Data.Data
import Data.Default
import Data.Default.Time
import Data.IxSet
import Data.SafeCopy
import Data.Time
import qualified Data.Budget.BudgetMigration as BEM
import Data.Budget.InternalMigration
import GHC.Generics hiding (to)
import qualified Data.Text as DT
import qualified Data.Text as DT

data BudgetItem = BudgetItem 
  { _budgetItemRate :: Rate
  , _budgetItemAmount :: BudgetAmount
  , _budgetName :: Name
  , _budgetItemBID :: BID
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

type BudgetStatusItem = (UTCTime, Amount, Amount)

type BudgetList = YabList BudgetItem

type BudgetDB = YabDB BudgetItem

type BudgetAudit = Audit BudgetItem

type BudgetAuditDB = AuditDB BudgetItem

makeClassy ''BudgetItem

$(deriveSafeCopy 1 'extension ''BudgetItem)

instance Default BudgetItem

instance Indexable BudgetItem where
  empty = ixSet
    [ ixFun $ (:[]) . (view bid)
    , ixFun $ (:[]) . (view budgetName)
    , ixFun $ (:[]) . (view amount)
    , ixFun $ (:[]) . (view amountType)
    ]

instance Migrate BudgetItem where
  type MigrateFrom BudgetItem = BEM.BudgetItem_v0
  migrate b = BudgetItem  
    { _budgetItemRate = Periodic $ BEM._budgetItemRate b
    , _budgetItemAmount = BEM._budgetItemAmount b
    , _budgetName = BEM._budgetName b
    , _budgetItemBID = BEM._budgetItemBID b
    }

instance HasBID BudgetItem where
  bid = budgetItemBID

instance HasName BudgetItem where
  name = budgetName

instance HasAmount BudgetItem where
  amount = budgetItemAmount

instance HasRate BudgetItem where
  rate = budgetItemRate
  
instance FromJSON BudgetItem where
  parseJSON v@(Object o) = BudgetItem 
    <$> o .: "rate" 
    <*> (parseJSON v)
    <*> o .: "name"
    <*> o .: "id"
  parseJSON _ = mempty

instance ToJSON BudgetItem where
  toJSON bi = object $ 
    budgetAmountJSON (bi^.budgetItemAmount)
    ++ ["rate" .= (bi^.rate)
       , "name" .= (bi^.name)
       , "id" .= (bi^.bid)
       ]
