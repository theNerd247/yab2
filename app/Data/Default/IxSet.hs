module Data.Default.IxSet where

import Data.IxSet
import Data.Default

instance (Indexable a) => Default (IxSet a) where
  def = empty
