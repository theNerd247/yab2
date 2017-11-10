{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Budget.InternalMigration where

import Data.SafeCopy
import Control.Lens
import Data.Data
import GHC.Generics
import Data.BIDMigration

type Rate_v0 = Int
