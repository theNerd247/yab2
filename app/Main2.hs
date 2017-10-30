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
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import CSV (parseDate)
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.State (get)
import Control.Applicative
import Data.Acid
import Data.Acid.Abstract
import Data.Aeson
import Data.Audit
import Data.Bank
import Data.Budget
import Data.Default (def)
import Data.IxSet
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Time (Day, fromGregorian)
import Data.Traversable (forM)
import Snap
import YabAcid
import Rest.Driver.Snap
import Api.Yab
import Api.ApiTypes
import Rest.Api
import Rest.Driver.Types


data App = App
  { _restSnaplet :: Snaplet YabApiData
  }

makeLenses ''App

appInit :: SnapletInit App App 
appInit = makeSnaplet "myapp" "Yab snaplet" Nothing $ do
  dbRef <- liftIO $ openLocalStateFrom "/tmp/tst" (def :: YabAcid)
  onUnload $ closeAcidState dbRef
  restApi <- nestSnaplet "yab" restSnaplet $ restInit (YabApiData dbRef) (\d -> liftIO . (runYabApi d)) api
  return $ App restApi

restInit :: (Monad m, Applicative m) => v -> (v -> Run m (Handler b v)) -> Api m -> SnapletInit b v
restInit initApiData run api = makeSnaplet "rest" "rest api snaplet" Nothing $ do
  addRoutes [("", ask >>= \d -> apiToHandler' (run d) api)]
  return initApiData

main = serveSnaplet defaultConfig appInit
