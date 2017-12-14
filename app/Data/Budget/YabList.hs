{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Budget.YabList where
  
import Data.Aeson
import Control.Lens hiding ((.=), Indexable)
import Data.IxSet
import Data.Data
import GHC.Generics hiding (to)
import Data.Budget.Name
import Data.Budget.Items
import Data.Budget.StartInfo
import Data.Default.IxSet
import Data.Default
import Control.Applicative ((<**>))

data YabList a = YabList
  { _yabListStartInfo :: StartInfo
  , _itemsDB :: IxSet a
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic) 

makeLenses ''YabList

instance (Indexable a) => Default (YabList a) where
  def = YabList { _yabListStartInfo = def
                , _itemsDB = def
                }

instance HasName (YabList a) where
  name = startInfo . name

instance HasStartInfo (YabList a) where
  startInfo = yabListStartInfo

instance (Ord a, Indexable a, Typeable a) => HasItems (YabList a) where
  type ItemType (YabList a) = a
  items = itemsDB . items

instance (Indexable a, Ord a, Typeable a, FromJSON a) => FromJSON (YabList a) where
  parseJSON v = pure def <**> parseItemsJSON v <**> parseStartInfoJSON v

instance (Ord a, Indexable a, Typeable a, ToJSON a) => ToJSON (YabList a) where
  toJSON b = object $ (toItemsJSON b) ++ (toStartInfoJSON b)

{-updateDBItems :: (HasYabDB r, MonadState s m, Ord (YabDBItem r), Indexable (YabDBItem r), Typeable k, Typeable (YabDBItem r)) => Lens' s r -> k -> [YabDBItem r] -> m ()-}
{-updateDBItems l key values = l.yabDB %= (appEndo (foldMap (Endo . updateIx key) values))-}
