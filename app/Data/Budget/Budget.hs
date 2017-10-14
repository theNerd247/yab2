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

import Control.Lens hiding ((.=))
import Data.Audit
import Data.BID
import Data.Budget.Internal
import Data.Data
import Data.DayDefault
import Data.Default
import Data.IxSet
import Data.SafeCopy
import Data.Time
import Data.Yaml hiding ((.~))
import qualified Data.Text as DT
import qualified Data.Text as DT

data BudgetItem = BudgetItem 
  { _rate :: Rate
  , _budgetItemBudgetAmount :: BudgetAmount
  , _budgetName :: Name
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

type BudgetList = YabList BudgetItem

type BudgetDB = YabDB BudgetItem

type BudgetAudit = Audit BudgetItem

makeClassy ''BudgetItem

$(deriveSafeCopy 0 'base ''BudgetItem)

instance Default BudgetItem

instance Indexable BudgetItem where
  empty = ixSet
    [ ixFun $ (:[]) . (view budgetName)
    , ixFun $ (:[]) . (view amount)
    , ixFun $ (:[]) . (view budgetType)
    ]

instance BudgetAtPeriod BudgetItem where
  budgetAtPeriod _ p = to gt
    where
      gt b = (b^.amount)*(fromIntegral . floor . toRational $ p `div` (b^.rate))

instance HasBudgetAmount BudgetItem where
  budgetAmount = budgetItemBudgetAmount
  
instance FromJSON BudgetItem where
  parseJSON v@(Object o) = BudgetItem 
    <$> o .: "rate" 
    <*> (parseJSON v)
  parseJSON _ = mempty

instance ToJSON BudgetItem where
  toJSON bi = object $ 
    budgetAmountJSON (bi^.budgetItemBudgetAmount)
    ++ ["rate" .= (bi^.rate)]
