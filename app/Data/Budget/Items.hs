{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Budget.Items where

import Control.Lens hiding ((.=), Indexable)
import Data.Aeson
import Data.Time
import Data.Data (Typeable)
import Data.IxSet
import qualified Data.List as DL

class HasDate a where
  date :: Lens' a UTCTime

class HasItems m where
  type ItemType m

  items :: Lens' m [ItemType m]
  items = items

instance HasItems [a] where
  type ItemType [a] = a
  items = id

instance (Ord a, Indexable a, Typeable a) => HasItems (IxSet a) where
  type ItemType (IxSet a) = a

  items = lens toList (\_ l -> fromList l)

parseItemsJSON (Object o) = do 
  is <- o .: "items" 
  return $ \v -> v & items .~ is
parseItemsJSON _ = mempty

toItemsJSON a = ["items" .= (a^.items)]

earliest :: (HasItems m, HasDate (ItemType m)) => Getter m (Maybe (ItemType m))
earliest = to $ \b -> earliest' (b^.items)
  where
    earliest' [] = Nothing
    earliest' xs = Just $ DL.minimumBy (\a b -> compare (a^.date) (b^.date)) xs

latest :: (HasItems m, HasDate (ItemType m)) => Getter m (Maybe (ItemType m))
latest = to $ \b -> latest' (b^.items)
  where
    latest' [] = Nothing
    latest' xs = Just $ DL.maximumBy (\a b -> compare (a^.date) (b^.date)) xs
