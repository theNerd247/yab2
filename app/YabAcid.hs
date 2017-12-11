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
import Data.Maybe (fromMaybe, fromJust)
import Data.Data
import Data.Default
import Data.Budget
import Data.SafeCopy
import Data.Default.IxSet
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

type YabDBT m a = StateT YabAcid m a

newtype AsYabListError = AsYabListError String
  deriving (Show)

instance Exception AsYabListError

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

  getItemsBy :: (MonadState YabAcid m) => (YabDB a -> YabDB a) -> m (YabDB a)
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

deletYabList nm = do
  mylist <- asYabList nm =<< getItemsBy (@= nm)
  flip (maybe $ return ()) mylist $ \ylist -> do
    forM (ylist^.items) deleteItem 
    deleteItem (ylist^.yabListStartInfo)

insertYabList :: (MonadIO m, MonadState YabAcid m, HasYabAcid a, Default a) => YabList a -> m (YabList a)
insertYabList l = getItemsBy (@= (l^.name)) >>= asYabList (l^.name) >>= maybe (mkNew l) (\bs -> return bs)
  where
    mkNew l = do
      si <- liftIO . setNewBID $ l^.startInfo
      is <- liftIO $ forM (l^.items) setNewBID
      insertItem si
      forM_ is insertItem
      return $ def 
        & startInfo .~ si
        & items .~ is

asYabList :: (HasStartInfo b, Ord (YabListItem b), HasYabList b, Default b, Typeable k, MonadState YabAcid m) => k -> IxSet (YabListItem b) -> m (Maybe b)
asYabList name db = do
  msinfo <- getItemsBy (getEQ name)
  return $ do
    sinfo <- earliestStartInfo $ toList msinfo
    return $ def
        & items .~ toList db
        & startInfo .~ sinfo

updateYabList :: (MonadIO m, Default a, MonadState YabAcid m, HasYabAcid a) => (YabList a) -> m (YabList a)
updateYabList newItems = do
  updateItem (newItems^.startInfo)
  oldItems <- toList <$> (getItemsBy $ getEQ $ newItems^.name)
  let updated  = DL.intersectBy elemSelect (newItems^.items) oldItems
  let inserted = DL.deleteFirstsBy elemSelect (newItems^.items) oldItems
  let deleted  = DL.deleteFirstsBy elemSelect oldItems (newItems^.items)
  forM_ updated  $ updateItem
  forM_ inserted $ insertItem
  forM_ deleted  $ deleteItem
  return . fromJust =<< asYabList (newItems^.name) =<< getItemsBy (getEQ $ newItems^.name)
  where
    elemSelect a b = a^.bid == b^.bid
