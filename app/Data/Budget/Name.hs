{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Budget.Name where

import Data.Aeson
import Control.Lens hiding ((.=))

type Name = String

class HasName m where
  name :: Lens' m Name

instance HasName Name where
  name = id

toNameJSON a = ["name" .= (a^.name)]

parseNameJSON (Object o) = do
  n <- o .: "name"
  return $ \v -> v & name .~ n
parseNameJSON _ = mempty
