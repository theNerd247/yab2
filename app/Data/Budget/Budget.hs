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
import Data.Budget.Rate
import Data.Budget.Name
import Data.Budget.Amount
import Data.Budget.YabList
import Data.Data
import Data.Default
import Data.Default.Time
import Data.IxSet
import Data.SafeCopy
import Data.Time
import GHC.Generics hiding (to)
import qualified Data.Text as DT
import qualified Data.Text as DT

data BudgetItem = BudgetItem 
  { _budgetItemRate :: Rate
  , _budgetItemAmount :: Amount
  , _budgetName :: Name
  , _budgetItemBID :: BID
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

type BudgetStatusItem = (UTCTime, Amount, Amount)

makeClassy ''BudgetItem

instance Default BudgetItem

instance Indexable BudgetItem where
  empty = ixSet
    [ ixFun $ (:[]) . (view bid)
    , ixFun $ (:[]) . (view budgetName)
    , ixFun $ (:[]) . (view amount)
    ]

data BudgetItem_v0 = BudgetItem_v0
  { _budgetItemRateV0 :: Rate_v0
  , _budgetItemBudgetAmountV0 :: BudgetAmount_v0
  , _budgetNameV0 :: Name
  , _budgetItemBIDV0 :: BID
  } deriving (Eq,Ord,Show,Read)

$(deriveSafeCopy 0 'base ''BudgetItem_v0)

data BudgetItem_v1 = BudgetItem_v1
  { _budgetItemRateV1 :: Rate
  , _budgetItemAmountV1 :: BudgetAmount_v0
  , _budgetNameV1 :: Name
  , _budgetItemBIDV1 :: BID
  } deriving (Eq,Ord,Show,Read)

instance Migrate BudgetItem_v1 where
  type MigrateFrom BudgetItem_v1 = BudgetItem_v0
  migrate b = BudgetItem_v1 
    { _budgetItemRateV1 = Periodic $ _budgetItemRateV0 b
    , _budgetItemAmountV1 = _budgetItemBudgetAmountV0 b
    , _budgetNameV1 = _budgetNameV0 b
    , _budgetItemBIDV1 = _budgetItemBIDV0 b
    }

$(deriveSafeCopy 1 'extension ''BudgetItem_v1)

instance Migrate BudgetItem where
  type MigrateFrom BudgetItem = BudgetItem_v1
  migrate b = BudgetItem
    { _budgetItemRate = _budgetItemRateV1 b
    , _budgetItemAmount = _budgetAmountAmountV0 $ _budgetItemAmountV1 b
    , _budgetName = _budgetNameV1 b
    , _budgetItemBID = _budgetItemBIDV1 b
    }

$(deriveSafeCopy 2 'extension ''BudgetItem)

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
    [ "amount" .= (bi^.amount)
    , "rate" .= (bi^.rate)
    , "name" .= (bi^.name)
    , "id" .= (bi^.bid)
    ]
