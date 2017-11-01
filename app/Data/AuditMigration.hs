{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.AuditMigration where

import Data.SafeCopy
import Control.Lens
import Data.Data
import GHC.Generics
import Data.BIDMigration
import Data.Time (UTCTime)

data Audit_v0 a = Audit_v0
  { _modTime :: UTCTime
  , _auditBID :: BID_v0
  , _modData :: a
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

$(deriveSafeCopy 0 'base ''Audit_v0)
makeLenses ''Audit_v0
