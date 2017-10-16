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
{-# LANGUAGE ExistentialQuantification #-}

module YabAcid where

import Data.Acid
import Data.Acid.Advanced
import Data.Audit
import Data.IxSet
import Data.Data
import Data.Budget
import Data.SafeCopy
import Control.Lens
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Data.Foldable (forM_)
import qualified Data.Map as DM

type YabAcidState = AcidState YabAcid

data YabAcid = YabAcid
  { _budgetDB :: BudgetDB
  , _budgetAuditDB :: BudgetAuditDB
  , _expenseDB :: ExpenseDB
  , _expenseAuditDB :: ExpenseAuditDB
  , _startInfoDB :: StartInfoDB
  } deriving (Eq,Ord,Show,Read,Data,Typeable)

makeLenses ''YabAcid

$(deriveSafeCopy 0 'base ''YabAcid)

addEList :: ExpenseList -> Update YabAcid ()
addEList es = updateYabDB expenseDB (listToDB es)

addEAudit :: (Audit ExpenseItem) -> Update YabAcid ()
addEAudit = addAuditItem expenseAuditDB

addBList :: BudgetList -> Update YabAcid ()
addBList bs = updateYabDB budgetDB (listToDB bs)

addBAudit :: (Audit BudgetItem) -> Update YabAcid ()
addBAudit = addAuditItem budgetAuditDB

insertE :: ExpenseItem -> Update YabAcid ()
insertE e = expenseDB %= insert e

insertB :: BudgetItem -> Update YabAcid ()
insertB e = budgetDB %= insert e

upsertEs :: ExpenseList -> Update YabAcid [[ExpenseItem]]
upsertEs es = do 
  db <- use expenseDB
  let (db,dups) = upsertExpenses (es^.items) db
  updateYabDB expenseDB db
  return dups

$(makeAcidic ''YabAcid ['addEList,'addEAudit,'addBList, 'addBAudit,'upsertEs])

withEAudit :: (MonadIO m) => YabAcidState -> ExpenseItem -> m ()
withEAudit db e = do 
  hist <- makeAudit e
  update' db $ AddEAudit hist

withBAudit :: (MonadIO m) => YabAcidState -> BudgetItem -> m ()
withBAudit db e = do 
  hist <- makeAudit e
  update' db $ AddBAudit hist

-- inserts a list of expense items into the db and adds audits to the expenses
addExpenseList :: (MonadIO m) => YabAcidState -> ExpenseList -> m ()
addExpenseList db es = do
  update' db $ AddEList es
  forM_ (es^.items) (withEAudit db)
  
addBudgetList :: (MonadIO m) => YabAcidState -> BudgetList -> m ()
addBudgetList db es = do
  update' db $ AddBList es
  forM_ (es^.items) (withBAudit db)

upsertExpenseList :: (MonadIO m) => YabAcidState -> ExpenseList -> m [[ExpenseItem]]
upsertExpenseList db es = update' db $ UpsertEs es
