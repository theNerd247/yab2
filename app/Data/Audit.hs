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

module Data.Audit where

import GHC.Generics hiding (to)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Data.Data
import Data.SafeCopy
import Data.Time
import Data.DayDefault
import Data.IxSet
import Data.Default
import Data.BID
import Data.Foldable (find)
import Control.Lens hiding ((.=), Indexable)
import Data.Traversable (forM)

type ModTime = UTCTime

data Audit a = Audit
  { _modTime :: ModTime
  , _auditBID :: BID
  , _modData :: a
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

type AuditDB a = IxSet (Audit a)

$(deriveSafeCopy 0 'base ''Audit)

class HasAudit m a | m -> a where
  audit :: Lens' m (Audit a)

  modTime :: Lens' m UTCTime
  modTime = audit . modTime

  auditBID :: Lens' m BID
  auditBID = audit . auditBID

  modData :: Lens' m a
  modData = audit . modData

class HasAuditDB m a | m -> a where
  auditDB :: Lens' m (AuditDB a)

instance HasAudit (Audit a) a where
  audit = id
  modTime = audit . go where go f (Audit t abid d) = (\t' -> Audit t' abid d) <$> f t
  auditBID = audit . go where go f (Audit t abid d) = (\abid' -> Audit t abid' d) <$> f abid
  modData = audit . go where go f (Audit t abid d) = (\d' -> Audit t abid d') <$> f d

instance HasBID (Audit a) where
  bid = auditBID

instance HasAuditDB (AuditDB a) a where
  auditDB = id

instance Indexable (Audit a) where
  empty = ixSet 
    [ ixFun $ modTimes
    , ixFun $ modBIDs
    ]
    where
      modTimes a = (:[]) $ a^.modTime
      modBIDs a = (:[]) $ a^.auditBID

makeAudit :: (MonadIO m) => a -> m (Audit a)
makeAudit x = liftIO $ do 
  now <- getCurrentTime
  aBID <- newBID
  return $ Audit
    { _modTime = now 
    , _auditBID = aBID
    , _modData = x
    }

queryAuditDB :: (HasAuditDB h s, MonadReader h m) => m (AuditDB s)
queryAuditDB = asks $ view auditDB

updateAuditDB :: (HasAuditDB h s, MonadState h m) => AuditDB s -> m ()
updateAuditDB = modify . (set auditDB)

addAuditItem :: (HasAuditDB s a, MonadState s m, Typeable a, Ord a) => (Audit a) -> m ()
addAuditItem x = auditDB %= (insert x)

getByBID :: (MonadReader (f a) m, HasBID a, HasBID b, Foldable f) => b -> m (Maybe a)
getByBID abid = asks $ findOf folded (^.bid.to (==abid^.bid))
