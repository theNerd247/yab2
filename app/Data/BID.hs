{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.BID where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Default
import Data.Data
import Data.SafeCopy
import Data.Digest.Pure.MD5
import Data.Time.Clock.POSIX
import GHC.Generics
import qualified Data.BIDMigration as BM
import qualified Data.Serialize as S

newtype BID = BID String
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

class HasBID a where
  bid :: Lens' a BID

instance HasBID BID where
  bid = id

instance S.Serialize POSIXTime where
  put = S.put . toRational
  get = fromRational <$> S.get

newBID :: IO BID
newBID = (BID . show . md5 . S.encodeLazy) <$> getPOSIXTime

setNewBID :: (HasBID a) => a -> IO a
setNewBID x = (\b -> x & bid .~ b) <$> newBID

$(deriveSafeCopy 1 'extension ''BID)

instance Migrate BID where
  type MigrateFrom BID = BM.BID_v0
  migrate = BID . show . md5 . S.encodeLazy

instance FromJSON BID

instance ToJSON BID

instance Default BID

toBIDJSON a = ["id" .= (a^.bid)]

parseBIDJSON (Object o) = do
  b <- o .: "id"
  return $ \v -> v & bid .~ b
