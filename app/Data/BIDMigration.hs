{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.BIDMigration where

import Data.SafeCopy
import GHC.Generics
import Data.Data
import qualified Data.Serialize as S
import Data.Digest.Pure.MD5

type BID_v0 = Int

newtype BID_v1 = BID_v1 String
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

$(deriveSafeCopy 1 'extension ''BID_v1)

instance Migrate BID_v1 where
  type MigrateFrom BID_v1 = BID_v0
  migrate = BID_v1 . show . md5 . S.encodeLazy
