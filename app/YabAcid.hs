{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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

type YabAcidLens a = Lens' YabAcid (YabDB a)

insertYabListItem :: (Indexable a, Ord a, Typeable a, MonadState YabAcid m) => YabAcidLens a -> a -> m ()
insertYabListItem l e = l %= insert e

updateYabListItem :: (MonadState YabAcid m, Indexable a, Ord a, Typeable a, HasBID a) => YabAcidLens a -> a -> m ()
updateYabListItem l b = l %= updateIx (b^.bid) b

deleteYabListItem :: (MonadState YabAcid m, Indexable a, Ord a, Typeable a, HasBID a) => YabAcidLens a -> a -> m ()
deleteYabListItem l b = l %= deleteIx (b^.bid)

updateDB :: YabAcid -> Update YabAcid ()
updateDB = put

getDB :: Query YabAcid YabAcid
getDB = ask

makeAcidic ''YabAcid ['updateDB, 'getDB]

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

  getItemsBy :: (MonadState YabAcid m) => (YabDB a -> YabDB a) -> m (YabDB a)
  getItemsBy f = gets $ view $ yabAcidLens.to f

  deleteItem :: (MonadState YabAcid m, MonadIO m) => a -> m ()
  deleteItem x = do
    hist <- makeAudit x
    modify $ \ydb -> ydb
      & yabAcidLens %~ delete x
      & yabAcidAuditLens %~ insert (hist & auditAction .~ Delete)

newItem :: (MonadState YabAcid m, MonadIO m, HasYabAcid a, Default a) => m a
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

asYabList :: (Default a) => (MonadState YabAcid m, HasYabAcid a) => Name -> YabDB a -> m (Maybe (YabList a))
asYabList name db = do
  msinfo <- getItemsBy (getEQ name)
  return $ do
    sinfo <- earliestStartInfo $ toList msinfo
    return $ def
        & items .~ toList db
        & startInfo .~ sinfo

updateYabList :: (MonadIO m, MonadState YabAcid m, HasYabAcid a) => (YabList a) -> m ()
updateYabList newItems = do
  updateItem (newItems^.startInfo)
  oldItems <- toList <$> (getItemsBy $ getEQ $ newItems^.name)
  let updated  = DL.intersectBy elemSelect (newItems^.items) oldItems
  let inserted = DL.deleteFirstsBy elemSelect (newItems^.items) oldItems
  let deleted  = DL.deleteFirstsBy elemSelect oldItems (newItems^.items)
  forM_ updated  $ updateItem
  forM_ inserted $ insertItem
  forM_ deleted  $ deleteItem
  where
    elemSelect a b = a^.bid == b^.bid


