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

module Data.BID where

import GHC.Generics hiding (to)
import Data.Data
import Data.Time.Clock.POSIX
import Data.Default
import Control.Lens hiding ((.=))
import Data.Hashable

data BID = BID
  { _bid :: !Int
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

makeClassy ''BID

instance Default BID

instance Hashable POSIXTime where
  hashWithSalt = hashUsing fromEnum 

newBID :: IO BID
newBID = hash <$> getPOSIXTime >>= return . (\x -> def & bid .~ x)
