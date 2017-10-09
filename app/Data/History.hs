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

module Data.History where

import GHC.Generics hiding (to)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Data.Data
import Data.SafeCopy
import Data.Time
import Data.DayDefault
import Data.Acid
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

data History a = History 
  { fromHistory :: (Audit a)
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)


type HistoryDB a = IxSet (History a)

$(deriveSafeCopy 0 'base ''BID)
$(deriveSafeCopy 0 'base ''Audit)
$(deriveSafeCopy 0 'base ''History)

class HasAudit m a | m -> a where
  audit :: Lens' m (Audit a)

  modTime :: Lens' m UTCTime
  modTime = audit . modTime

  auditBID :: Lens' m BID
  auditBID = audit . auditBID

  modData :: Lens' m a
  modData = audit . modData

class HasHistoryDB m a | m -> a where
  historyDB :: Lens' m (HistoryDB a)
  
instance HasAudit (Audit a) a where
  audit = id
  modTime = audit . go where go f (Audit t abid d) = (\t' -> Audit t' abid d) <$> f t
  auditBID = audit . go where go f (Audit t abid d) = (\abid' -> Audit t abid' d) <$> f abid
  modData = audit . go where go f (Audit t abid d) = (\d' -> Audit t abid d') <$> f d

instance HasBID (Audit a) where
  bID = auditBID

instance HasHistoryDB (HistoryDB a) a where
  historyDB = id

instance Indexable (History a) where
  empty = ixSet 
    [ ixFun $ modTimes
    , ixFun $ modBIDs
    ]
    where
      modTimes (History a) = (:[]) $ a^.modTime
      modBIDs (History a) = (:[]) $ a^.auditBID

makeAuditData :: (MonadIO m) => a -> m (Audit a)
makeAuditData x = liftIO $ do 
  now <- getCurrentTime
  aBID <- newBID
  return $ Audit
    { _modTime = now 
    , _auditBID = aBID
    , _modData = x
    }

makeHistory :: (MonadIO m) => a -> m (History a)
makeHistory = fmap History . makeAuditData

queryHistoryDB :: (HasHistoryDB h s, MonadReader h m) => m (HistoryDB s)
queryHistoryDB = asks $ view historyDB

updateHistoryDB :: (HasHistoryDB h s, MonadState h m) => HistoryDB s -> m ()
updateHistoryDB = modify . (set historyDB)

addHistoryItem :: (HasHistoryDB s a, MonadState s m, MonadIO m, Typeable a, Ord a) => a -> m ()
addHistoryItem x = do
  hist <- liftIO $ makeHistory x
  historyDB %= (insert hist)

getByBID :: (MonadReader (f a) m, HasBID a, HasBID b, Foldable f) => b -> m (Maybe a)
getByBID abid = asks $ findOf folded (^.bid.to (==abid^.bid))
