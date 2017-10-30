{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.ApiTypes where

import YabAcid 
import Control.Lens
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Trans (MonadIO)
 
data YabApiData = YabApiData
  { _db :: YabAcidState
  }

makeLenses ''YabApiData

newtype YabApi a = YabApi { unYabApi :: ReaderT YabApiData IO a}
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader YabApiData)

runYabApi :: YabApiData -> YabApi a -> IO a
runYabApi yabData = flip runReaderT yabData . unYabApi
