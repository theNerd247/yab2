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
import Data.Audit
import Data.Acid
import Data.BID
import Data.Budget.Internal
import Data.Data
import Data.Default.Time
import Data.Default
import Data.IxSet
import Data.SafeCopy
import Data.Time
import GHC.Generics hiding (to)
import Data.Aeson hiding ((.~))
import qualified Data.Text as DT
import qualified Data.Text as DT

data BudgetItem = BudgetItem 
  { _rate :: Rate
  , _budgetItemBudgetAmount :: BudgetAmount
  , _budgetName :: Name
  , _budgetItemBID :: BID
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

type BudgetList = YabList BudgetItem

type BudgetDB = YabDB BudgetItem

type BudgetAudit = Audit BudgetItem

type BudgetAuditDB = AuditDB BudgetItem

makeClassy ''BudgetItem

$(deriveSafeCopy 0 'base ''BudgetItem)

instance Default BudgetItem

instance Indexable BudgetItem where
  empty = ixSet
    [ ixFun $ (:[]) . (view bid)
    , ixFun $ (:[]) . (view budgetName)
    , ixFun $ (:[]) . (view amount)
    , ixFun $ (:[]) . (view amountType)
    ]

instance BudgetAtPeriod BudgetItem where
  budgetAmountAtPeriod _ p = to gt
    where
      gt b = (b^.amount)*(fromIntegral . floor . toRational $ p `div` (b^.rate))

instance HasBID BudgetItem where
  bid = budgetItemBID

instance HasName BudgetItem where
  name = budgetName

instance HasBudgetAmount BudgetItem where
  budgetAmount = budgetItemBudgetAmount
  
instance FromJSON BudgetItem where
  parseJSON v@(Object o) = BudgetItem 
    <$> o .: "rate" 
    <*> (parseJSON v)
    <*> o .: "name"
    <*> o .: "id"
  parseJSON _ = mempty

instance ToJSON BudgetItem where
  toJSON bi = object $ 
    budgetAmountJSON (bi^.budgetItemBudgetAmount)
    ++ ["rate" .= (bi^.rate)
       , "name" .= (bi^.name)
       , "id" .= (bi^.bid)
       ]
