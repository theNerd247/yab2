{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module YabAcid where

import Data.Acid
import Data.Acid.Advanced
import Data.Audit
import Data.IxSet
import Data.Time (Day)
import Data.Maybe (fromMaybe, fromJust)
import Data.Data
import Data.Default
import Data.Budget
import Data.SafeCopy
import Data.Default.IxSet hiding (Proxy)
import GHC.Generics hiding (to)
import Control.Lens hiding (Indexable)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Exception
import Data.Foldable (forM_,fold)
import Data.Traversable (forM)
import Data.List ((\\))
import qualified Data.Map as DM
import qualified Data.List as DL

type YabAcidState = AcidState YabAcid

data YabAcid = YabAcid
  { _budgetDB :: IxSet BudgetItem
  , _budgetAuditDB :: IxSet (Audit BudgetItem)
  , _expenseDB :: IxSet ExpenseItem
  , _expenseAuditDB :: IxSet (Audit ExpenseItem)
  , _startInfoDB :: IxSet StartInfo
  , _startInfoAuditDB :: IxSet (Audit StartInfo)
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

makeLenses ''YabAcid

$(deriveSafeCopy 0 'base ''YabAcid)

instance Default YabAcid

type YabAcidLens a = Lens' YabAcid (IxSet a)

type YabDBT m a = StateT YabAcid m a

newtype AsYabListError = AsYabListError String
  deriving (Show)

instance Exception AsYabListError

updateDB :: YabAcid -> Update YabAcid ()
updateDB = put

getDB :: Query YabAcid YabAcid
getDB = ask

makeAcidic ''YabAcid ['updateDB, 'getDB]

withYABDB :: (MonadIO m) => YabAcidState -> YabDBT m a -> m a
withYABDB db x = do
  ydb <- query' db GetDB
  (a,s) <- runStateT x ydb
  update' db $ UpdateDB s
  return a

class (HasBID a, HasName a, Indexable a, Typeable a, Ord a) => HasYabAcid a where
  yabAcidLens :: YabAcidLens a
  yabAcidAuditLens :: YabAcidLens (Audit a)

  insertItem :: (MonadState YabAcid m, MonadIO m) => a -> m ()
  insertItem newX = do
    x <- liftIO $ setNewBID newX
    hist <- makeAudit x
    modify $ \ydb -> ydb
      & yabAcidLens %~ insert x
      & yabAcidAuditLens %~ insert (hist & auditAction .~ Create)

  updateItem :: (MonadState YabAcid m, MonadIO m) => a -> m ()
  updateItem x = do
    hist <- makeAudit x
    modify $ \ydb -> ydb
      & yabAcidLens %~ updateIx (x^.bid) x
      & yabAcidAuditLens %~ insert (hist & auditAction .~ Modify)

  getItemsBy :: (MonadState YabAcid m) => (IxSet a -> IxSet a) -> m (IxSet a)
  getItemsBy f = gets $ view $ yabAcidLens.to f

  deleteItem :: (MonadState YabAcid m, MonadIO m) => a -> m ()
  deleteItem x = do
    hist <- makeAudit x
    modify $ \ydb -> ydb
      & yabAcidLens %~ delete x
      & yabAcidAuditLens %~ insert (hist & auditAction .~ Delete)

newItem :: (MonadState YabAcid m, HasYabAcid a, MonadIO m, Default a) => m a
newItem = let x = def in insertItem x >> return x

instance HasYabAcid BudgetItem where
  yabAcidLens = budgetDB
  yabAcidAuditLens = budgetAuditDB

instance HasYabAcid ExpenseItem where
  yabAcidLens = expenseDB
  yabAcidAuditLens = expenseAuditDB

instance HasYabAcid StartInfo where
  yabAcidLens = startInfoDB
  yabAcidAuditLens = startInfoAuditDB

deleteYabList ylist = do
  forM (ylist^.items) deleteItem 
  deleteItem (ylist^.yabListStartInfo)

insertYabList l = getItemsBy (@= (l^.name)) 
  >>= asYabList (l^.name) 
  >>= maybe (mkNew l) (\bs -> return bs)
  where
    mkNew l = do
      si <- liftIO . setNewBID $ l^.startInfo
      is <- liftIO $ forM (l^.items) setNewBID
      insertItem si
      forM_ is insertItem
      return $ def 
        & startInfo .~ si
        & items .~ is

asYabList name db = do
  msinfo <- getItemsBy (@= name)
  return $ do
    sinfo <- msinfo^.earliest :: Maybe StartInfo
    return $ YabList
      { _itemsDB = db
      , _yabListStartInfo = sinfo
      }

updateYabList newItems = do
  updateItem (newItems^.startInfo)
  oldItems <- getItemsBy (getEQ $ newItems^.name)
  let ois = oldItems^.items
  let nis = newItems^.items
  let updated  = DL.intersectBy elemSelect nis ois
  let inserted = DL.deleteFirstsBy elemSelect nis ois
  let deleted  = DL.deleteFirstsBy elemSelect ois nis
  forM_ updated  $ updateItem
  forM_ inserted $ insertItem
  forM_ deleted  $ deleteItem
  return newItems
  where
    elemSelect a b = a^.bid == b^.bid
