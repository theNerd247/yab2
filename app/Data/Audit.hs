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
import Data.Aeson
import Data.Data
import Data.SafeCopy
import Data.Time
import Data.Default.Time
import Data.IxSet
import Data.Default
import Data.BID
import Data.Foldable (find)
import Control.Lens hiding ((.=), Indexable)
import Data.Traversable (forM)

type ModTime = UTCTime

data AuditAction = Create | Modify | Delete
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data Audit a = Audit
  { _modTime :: ModTime
  , _auditBID :: BID
  , _modData :: a
  , _auditAction :: AuditAction
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

type AuditDB a = IxSet (Audit a)

$(deriveSafeCopy 0 'base ''AuditAction)

$(deriveSafeCopy 0 'base ''Audit)

class HasAudit m a | m -> a where
  audit :: Lens' m (Audit a)

  modTime :: Lens' m UTCTime
  modTime = audit . modTime

  auditBID :: Lens' m BID
  auditBID = audit . auditBID

  modData :: Lens' m a
  modData = audit . modData

  auditAction :: Lens' m AuditAction
  auditAction = audit . auditAction

class HasAuditDB m a | m -> a where
  auditDB :: Lens' m (AuditDB a)

instance HasAudit (Audit a) a where
  audit = id
  modTime = audit . go where go f (Audit t abid d a) = (\t' -> Audit t' abid d a) <$> f t
  auditBID = audit . go where go f (Audit t abid d a) = (\abid' -> Audit t abid' d a) <$> f abid
  modData = audit . go where go f (Audit t abid d a) = (\d' -> Audit t abid d' a) <$> f d
  auditAction = audit . go where go f (Audit t abid d a) = (\a' -> Audit t abid d a') <$> f a

instance HasBID (Audit a) where
  bid = auditBID

instance HasAuditDB (AuditDB a) a where
  auditDB = id

instance Indexable (Audit a) where
  empty = ixSet 
    [ ixFun $ modTimes
    , ixFun $ modBIDs
    , ixFun $ action
    ]
    where
      modTimes a = (:[]) $ a^.modTime
      modBIDs a = (:[]) $ a^.auditBID
      action a = (:[]) $ a^.auditAction

instance FromJSON AuditAction 

instance (FromJSON a) => FromJSON (Audit a)

instance ToJSON AuditAction 

instance (ToJSON a) => ToJSON (Audit a)

makeAudit :: (MonadIO m) => a -> m (Audit a)
makeAudit x = liftIO $ do 
  now <- getCurrentTime
  aBID <- newBID
  return $ Audit
    { _modTime = now 
    , _auditBID = aBID
    , _modData = x
    , _auditAction = Modify
    }
