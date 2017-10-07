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
import Data.Data
import Data.Time
import Data.DayDefault
import Data.Default
import Data.BID
import Control.Lens hiding ((.=))
import Data.Traversable (forM)

data Audit a = Audit
  { _modTime :: UTCTime
  , _auditBID :: BID
  , _modData :: a
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data History = forall a. History { fromHistory :: (Audit a)}

class HasAudit m a | m -> a where
  audit :: Lens' m (Audit a)

  modTime :: Lens' m UTCTime
  modTime = audit . modTime

  auditBID :: Lens' m BID
  auditBID = audit . auditBID

  modData :: Lens' m a
  modData = audit . modData

instance HasAudit (Audit a) a where
  audit = id
  modTime = audit . go where go f (Audit t abid d) = (\t' -> Audit t' abid d) <$> f t
  auditBID = audit . go where go f (Audit t abid d) = (\abid' -> Audit t abid' d) <$> f abid
  modData = audit . go where go f (Audit t abid d) = (\d' -> Audit t abid d') <$> f d

instance HasBID (Audit a) where
  bID = auditBID

makeAuditData :: a -> IO (Audit a)
makeAuditData x = do 
  now <- getCurrentTime
  aBID <- newBID
  return $ Audit
    { _modTime = now 
    , _auditBID = aBID
    , _modData = x
    }

makeHistory :: a -> IO History
makeHistory = fmap History . makeAuditData
