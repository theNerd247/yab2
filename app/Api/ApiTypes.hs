{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Api.ApiTypes where

import YabAcid 
import Control.Lens
import Snap.Snaplet
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
 
type YabSnapletLens b = Lens' b YabAcidState

class HasYabAcidSnaplet b where
  snapletYabAcid :: YabSnapletLens b

{-withYabSnapletDB :: (MonadSnaplet m, MonadIO (m b YabSnaplet), MonadReader Y, HasYabAcidSnaplet b) => YabDBT (m b YabSnaplet) a -> m b v a-}

withYabSnapletDB :: (HasYabAcidSnaplet b, MonadSnaplet m, MonadIO (m b b), MonadReader b (m b b)) => YabDBT (m b b) a -> m b v a
withYabSnapletDB f = withTop' id $ do
  s <- ask
  withYABDB (s^.snapletYabAcid) f
