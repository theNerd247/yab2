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
import GHC.Generics
import Control.Lens hiding (Indexable)
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
      updateYabDB expenseDB (db `union` newDB)
      return dups
      where
        smallDB = db @>=<= ((earliestExpense es)^.expenseDate, (latestExpense es)^.expenseDate)
    exs = DL.groupBy (\a b -> a^.name == b^.name) . DL.sortBy (\a b -> compare (a^.name) (b^.name)) $ es

getSIByName :: Name -> Query YabAcid StartInfoDB
getSIByName n = queryDBItems startInfoDB (@= n)

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
  'insertE
  ,'insertB
  ,'insertSI
  ,'insertEAudit
  ,'insertBAudit
  ,'insertSAudit
  ,'upsertEs
  ,'getBByName
  ,'getEsByDate
  ,'getEsByAmount
  ,'getEsByReason
  ,'getEsByName
  ,'getEsByBID
  ,'getSIByName
  ])

insertExpenseItem db e = do
  hist <- makeAudit e
  update' db $ InsertE e
  update' db $ InsertEAudit hist

insertBudgetItem db e = do
  hist <- makeAudit e
  update' db $ InsertB e
  update' db $ InsertBAudit hist

insertStartInfo db s = do
  hist <- makeAudit s
  update' db $ InsertSI s
  update' db $ InsertSAudit hist

-- inserts a list of expense items into the db and adds audits to the expenses
insertExpenseList db es = do
  forM_ (es^.items) (insertExpenseItem db)
  insertStartInfo db (es^.startInfo)
  
insertBudgetList db es = do
  forM_ (es^.items) (insertBudgetItem db)
  insertStartInfo db (es^.startInfo)

mergeExpenses db = update' db . UpsertEs

getBudgetByName db  = query' db . GetBByName

getExpensesByDate db s = query' db . GetEsByDate s

getExpensesByAmount db = query' db . GetEsByAmount

getExpensesByReason db = query' db . GetEsByReason

getExpensesByName db = query' db . GetEsByName

getExpensesByBID db = query' db . GetEsByBID

getStartInfoByName db = query' db . GetSIByName

asYabList :: (MonadIO m, HasName a, Typeable a, Ord a, Indexable a, Default a) => YabAcidState -> Name -> (m (YabDB a)) -> m (Maybe (YabList a))
asYabList db name ydbQuery = do
  -- get the earliest start info for the budget
  msinfo <- getStartInfoByName db name
  mydb <- (@= name) <$> ydbQuery 
  return $ do
    sinfo <- err . toList $ msinfo
    ydb <- err . toList $ mydb
    return $ def 
        & items .~ ydb
        & startInfo .~ DL.minimumBy (\a b -> (a^.startDate) `compare` (b^.startDate)) sinfo
  where
    err [] = Nothing
    err xs = Just xs
