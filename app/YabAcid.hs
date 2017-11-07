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
import Data.Maybe (fromMaybe)
import Data.Data
import Data.Default
import Data.Budget
import Data.SafeCopy
import Data.Default.IxSet
import GHC.Generics hiding (to)
import Control.Lens hiding (Indexable)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Data.Foldable (forM_,fold)
import Data.Traversable (forM)
import Data.List ((\\))
import qualified Data.Map as DM
import qualified Data.List as DL

type YabAcidState = AcidState YabAcid

data YabAcid = YabAcid
  { _budgetDB :: BudgetDB
  , _budgetAuditDB :: BudgetAuditDB
  , _expenseDB :: ExpenseDB
  , _expenseAuditDB :: ExpenseAuditDB
  , _startInfoDB :: StartInfoDB
  , _startInfoAuditDB :: StartInfoAuditDB
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

makeLenses ''YabAcid

$(deriveSafeCopy 0 'base ''YabAcid)

instance Default YabAcid

insertEAudit :: (Audit ExpenseItem) -> Update YabAcid ()
insertEAudit x = expenseAuditDB %= insert x

insertBAudit :: (Audit BudgetItem) -> Update YabAcid ()
insertBAudit x = budgetAuditDB %= insert x

insertSAudit :: (Audit StartInfo) -> Update YabAcid ()
insertSAudit x = startInfoAuditDB %= insert x

insertE :: ExpenseItem -> Update YabAcid ()
insertE e = expenseDB %= insert e

insertB :: BudgetItem -> Update YabAcid ()
insertB e = budgetDB %= insert e

insertSI :: StartInfo -> Update YabAcid ()
insertSI si = startInfoDB %= insert si

upsertEs :: [ExpenseItem] -> Update YabAcid [[ExpenseItem]]
upsertEs es = do 
  db <- use expenseDB
  -- upsert each expense item individually
  dups <- forM exs (upsertEs' db)
  return $ fold dups
  where 
    upsertEs' db es = do
      let (newDB,dups) = upsertExpenses es smallDB
      expenseDB .= (db `union` newDB)
      return dups
      where
        smallDB = db @>=<= ((earliestExpense es)^.expenseDate, (latestExpense es)^.expenseDate)
    exs = DL.groupBy (\a b -> a^.name == b^.name) . DL.sortBy (\a b -> compare (a^.name) (b^.name)) $ es

getSIByName :: Name -> Query YabAcid StartInfoDB
getSIByName n = asks . view $ startInfoDB.to (@= n)

getEsByDate :: UTCTime -> UTCTime -> Query YabAcid ExpenseDB
getEsByDate start end = asks . view $ expenseDB.to (@>=<= (start,end))

getEsByNameAndDates :: Name -> UTCTime -> UTCTime -> Query YabAcid ExpenseDB
getEsByNameAndDates n s e = asks . view $ expenseDB.to ((@= n) . (@>=<= (s,e)))

getEsByAmount :: Amount -> Query YabAcid ExpenseDB
getEsByAmount a = asks . view $ expenseDB.to (@= a)

getEsByReason :: String -> Query YabAcid ExpenseDB
getEsByReason a = asks . view $ expenseDB.to (@= a)

getEsByName :: Name -> Query YabAcid ExpenseDB
getEsByName a = asks . view $ expenseDB.to (@= a)

getEsByBID :: BID -> Query YabAcid ExpenseDB
getEsByBID a = asks . view $ expenseDB.to (@= a)

getBByName :: Name -> Query YabAcid BudgetDB
getBByName n = asks . view $ budgetDB.to (@= n)

getBByNameAndDates :: Name -> UTCTime -> UTCTime -> Query YabAcid BudgetDB
getBByNameAndDates n s e = asks . view $ budgetDB.to ((@= n) . (@>=<= (s,e)))

updateB :: BudgetItem -> Update YabAcid ()
updateB b = budgetDB %= updateIx (b^.bid) b

updateE :: ExpenseItem -> Update YabAcid ()
updateE e = expenseDB %= updateIx (e^.bid) e

updateSI :: StartInfo -> Update YabAcid ()
updateSI si = startInfoDB %= updateIx (si^.bid) si

getAllBNames :: Query YabAcid [Name]
getAllBNames = do 
  bs <- asks . view $ startInfoDB
  return $ (groupBy bs)^..traversed._1

deleteBByBID :: BID -> Update YabAcid ()
deleteBByBID bid = budgetDB %= deleteIx bid

deleteEByBID :: BID -> Update YabAcid ()
deleteEByBID bid = expenseDB %= deleteIx bid

deleteSIByBID :: BID -> Update YabAcid ()
deleteSIByBID bid = startInfoDB %= deleteIx bid

$(makeAcidic ''YabAcid [
  'insertE
  ,'insertB
  ,'insertSI
  ,'insertEAudit
  ,'insertBAudit
  ,'insertSAudit
  ,'upsertEs
  ,'getBByName
  ,'getBByNameAndDates
  ,'getEsByDate
  ,'getEsByAmount
  ,'getEsByReason
  ,'getEsByName
  ,'getEsByNameAndDates
  ,'getEsByBID
  ,'getSIByName
  ,'updateSI
  ,'updateE
  ,'updateB
  ,'getAllBNames
  ,'deleteBByBID
  ,'deleteEByBID
  ,'deleteSIByBID
  ])

insertExpenseItem db e = do
  hist <- makeAudit e
  update' db $ InsertE e
  update' db $ InsertEAudit $ hist & auditAction .~ Create

insertBudgetItem db e = do
  hist <- makeAudit e
  update' db $ InsertB e
  update' db $ InsertBAudit $ hist & auditAction .~ Create

insertStartInfo db s = do
  hist <- makeAudit s
  update' db $ InsertSI s
  update' db $ InsertSAudit $ hist & auditAction .~ Create

-- inserts a list of expense items into the db and adds audits to the expenses
insertExpenseList db es = do
  forM_ (es^.items) (insertExpenseItem db)
  insertStartInfo db (es^.startInfo)
  
insertBudgetList db es = do
  forM_ (es^.items) (insertBudgetItem db)
  insertStartInfo db (es^.startInfo)

updateBudgetItem db b = do 
  hist <- makeAudit b
  update' db $ InsertBAudit $ hist & auditAction .~ Modify
  update' db $ UpdateB b

updateExpenseItem db e = do 
  hist <- makeAudit e
  update' db $ InsertEAudit $ hist & auditAction .~ Modify
  update' db $ UpdateE e

updateStartInfo db s = do 
  hist <- makeAudit s
  update' db $ InsertSAudit $ hist & auditAction .~ Modify
  update' db $ UpdateSI s

deleteBudgetItem db b = do
  hist <- makeAudit b
  update' db $ InsertBAudit $ hist & auditAction .~ Delete
  update' db $ DeleteBByBID (b^.bid)

deleteExpenseItem db b = do
  hist <- makeAudit b
  update' db $ InsertEAudit $ hist & auditAction .~ Delete
  update' db $ DeleteEByBID (b^.bid)

deleteStartInfo db b = do
  hist <- makeAudit b
  update' db $ InsertSAudit $ hist & auditAction .~ Delete
  update' db $ DeleteSIByBID (b^.bid)

mergeExpenses db = update' db . UpsertEs

getBudgetByName db  = query' db . GetBByName

getBudgetByNameAndDates db n d = query' db . GetBByNameAndDates n d

getExpensesByDate db s = query' db . GetEsByDate s

getExpensesByAmount db = query' db . GetEsByAmount

getExpensesByReason db = query' db . GetEsByReason

getExpensesByName db = query' db . GetEsByName

getExpensesByNameAndDates db n d = query' db . GetEsByNameAndDates n d

getExpensesByBID db = query' db . GetEsByBID

getStartInfoByName db = query' db . GetSIByName

getAllBudgetNames db = query' db GetAllBNames

asYabList :: (MonadIO m, HasName a, Typeable a, Ord a, Indexable a, Default a) => YabAcidState -> Name -> (m (YabDB a)) -> m (Maybe (YabList a))
asYabList db name ydbQuery = do
  -- get the earliest start info for the budget
  msinfo <- getStartInfoByName db name
  mydb <- (@= name) <$> ydbQuery 
  return $ do
    sinfo <- earliestStartInfo $ toList msinfo
    return $ def 
        & items .~ toList mydb
        & startInfo .~ sinfo

updateBudgetList :: (MonadIO m) => YabAcidState -> BudgetList -> m ()
updateBudgetList db newItems = do
  updateStartInfo db (newItems^.startInfo)
  oldItems <- toList <$> (getBudgetByName db (newItems^.name))
  let updated = DL.intersectBy (\a b -> a^.bid == b^.bid) (newItems^.items) oldItems
  let inserted = (newItems^.items) \\ oldItems
  let deleted = oldItems \\ (newItems^.items)
  forM_ updated $ updateBudgetItem db
  forM_ inserted $ insertBudgetItem db
  forM_ deleted $ deleteBudgetItem db

updateExpenseList :: (MonadIO m) => YabAcidState -> ExpenseList -> m ()
updateExpenseList db newItems = do
  updateStartInfo db (newItems^.startInfo)
  oldItems <- toList <$> (getExpensesByName db (newItems^.name))
  let updated = DL.intersectBy (\a b -> a^.bid == b^.bid) (newItems^.items) oldItems
  let inserted = (newItems^.items) \\ oldItems
  let deleted = oldItems \\ (newItems^.items)
  forM_ updated $ updateExpenseItem db
  forM_ inserted $ insertExpenseItem db
  forM_ deleted $ deleteExpenseItem db
