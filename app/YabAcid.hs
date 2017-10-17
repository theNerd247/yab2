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
{-# LANGUAGE TypeSynonymInstances #-}

module YabAcid where

import Data.Acid
import Data.Acid.Advanced
import Data.Audit
import Data.IxSet
import Data.Time (Day)
import Data.Data
import Data.Default
import Data.Budget
import Data.SafeCopy
import Data.Default.IxSet
import GHC.Generics
import Control.Lens
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Data.Foldable (forM_,fold)
import Data.Traversable (forM)
import qualified Data.Map as DM
import qualified Data.List as DL

type YabAcidState = AcidState YabAcid

data YabAcid = YabAcid
  { _budgetDB :: BudgetDB
  , _budgetAuditDB :: BudgetAuditDB
  , _expenseDB :: ExpenseDB
  , _expenseAuditDB :: ExpenseAuditDB
  , _startInfoDB :: StartInfoDB
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

makeLenses ''YabAcid

$(deriveSafeCopy 0 'base ''YabAcid)

instance Default YabAcid

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

upsertEs :: [ExpenseItem] -> Update YabAcid [[ExpenseItem]]
upsertEs es = do 
  db <- use expenseDB
  dups <- forM exs (upsertEs' db)
  return $ fold dups
  where 
    upsertEs' db es = do
      let (newDB,dups) = upsertExpenses es smallDB
      updateYabDB expenseDB (db `union` newDB)
      return dups
      where
        smallDB = db @>=<= ((earliestExpense es)^.expenseDate, (latestExpense es)^.expenseDate)
    exs = DL.groupBy (\a b -> a^.name == b^.name) . DL.sortBy (\a b -> compare (a^.name) (b^.name)) $ es

querySInfo :: Name -> Query YabAcid (Maybe StartInfo)
querySInfo n = queryDBItems startInfoDB (getEQ n) >>= return . getOne

getEsByDate :: Day -> Day -> Query YabAcid ExpenseDB
getEsByDate start end = queryDBItems expenseDB (@>=<= (start,end))

getEsByAmount :: Amount -> Query YabAcid ExpenseDB
getEsByAmount a = queryDBItems expenseDB (@= a)

getEsByReason :: String -> Query YabAcid ExpenseDB
getEsByReason a = queryDBItems expenseDB (@= a)

getEsByName :: Name -> Query YabAcid ExpenseDB
getEsByName a = queryDBItems expenseDB (@= a)

getEsByBID :: BID -> Query YabAcid ExpenseDB
getEsByBID a = queryDBItems expenseDB (@= a)

getBByName :: Name -> Query YabAcid BudgetDB
getBByName n = queryYabDB budgetDB >>= return . getEQ n

$(makeAcidic ''YabAcid [
  'addEList
  ,'addEAudit
  ,'addBList
  ,'addBAudit
  ,'upsertEs
  ,'getBByName
  ,'getEsByDate
  ,'getEsByAmount
  ,'getEsByReason
  ,'getEsByName
  ,'getEsByBID
  ,'querySInfo
  ])

withEAudit db e = do 
  hist <- makeAudit e
  update' db $ AddEAudit hist

withBAudit db e = do 
  hist <- makeAudit e
  update' db $ AddBAudit hist

-- inserts a list of expense items into the db and adds audits to the expenses
addExpenseList db es = do
  update' db $ AddEList es
  forM_ (es^.items) (withEAudit db)
  
addBudgetList db es = do
  update' db $ AddBList es
  forM_ (es^.items) (withBAudit db)

mergeExpenses db = update' db . UpsertEs

getBudgetyName db  = query' db . GetBByName

getExpensesByDate s db = query' db . GetEsByDate s

getExpensesByAmount db = query' db . GetEsByAmount

getExpensesByReason db = query' db . GetEsByReason

getExpensesByName db = query' db . GetEsByName

getExpensesByBID db = query' db . GetEsByBID

queryStartInfo db = query' db . QuerySInfo
