{-# LANGUAGE TypeSynonymInstances  #-}

module Data.BID where

import Data.Time.Clock.POSIX
import Control.Lens
import Data.Hashable

type BID = Int

class HasBID a where
  bid :: Lens' a BID

instance HasBID BID where
  bid = id

instance Hashable POSIXTime where
  hashWithSalt = hashUsing fromEnum 

newBID :: IO BID
newBID = hash <$> getPOSIXTime

setNewBID :: (HasBID a) => a -> IO a
setNewBID x = (\b -> x & bid .~ b) <$> newBID
