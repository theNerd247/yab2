module Data.BIDMigration where

import Data.SafeCopy
import Control.Lens
import Data.Data
import GHC.Generics
import Data.Time (UTCTime)

type BID_v0 = Int
