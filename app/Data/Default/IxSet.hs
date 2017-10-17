module Data.Default.IxSet where

import Data.IxSet
import Data.Data
import Data.Default

instance (Indexable a, Ord a, Typeable a) => Default (IxSet a) where
  def = fromList []
