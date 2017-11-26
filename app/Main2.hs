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
import Snap.Util.FileUploads
import YabAcid
import Rest.Driver.Snap
import Api.Yab
import Api.ApiTypes hiding (db)
import Rest.Api
import Rest.Driver.Types
import qualified Data.ByteString.Char8 as B

data App = App
  { db :: YabAcidState
  , _restSnaplet :: Snaplet YabApiData
  }

makeLenses ''App

appInit :: SnapletInit App App 
appInit = makeSnaplet "myapp" "Yab snaplet" Nothing $ do
  dbRef <- liftIO $ openLocalStateFrom "/tmp/tst" (def :: YabAcid)
  onUnload $ closeAcidState dbRef
  restApi <- nestSnaplet "yab" restSnaplet $ restInit (YabApiData dbRef) (\d -> liftIO . runYabApi d) api
  addRoutes [("transaction/:name", uploadTransaction)]
  return $ App dbRef restApi

restInit :: (Monad m, Applicative m) => v -> (v -> Run m (Handler b v)) -> Api m -> SnapletInit b v
restInit initApiData run api = makeSnaplet "rest" "rest api snaplet" Nothing $ do
  addRoutes [("", ask >>= \d -> apiToHandler' (run d) api)]
  return initApiData

uploadTransaction :: Handler b App ()
uploadTransaction = do
  n <- getParam "name"
  maybe (withErr "no param called name") upload n
  where 
    upload name = do
      dups <- uploadCSVFiles $ \f -> do
        db <- asks db
        es <- loadNewTransactionFile (B.unpack name) f
        mergeExpenses db es
      asJSON $ mconcat dups

uploadCSVFiles :: (FilePath -> Handler b App a) -> Handler b App [a]
uploadCSVFiles f = withTemporaryStore "/tmp" "yab-" $ \store -> do
    (inputs, files) <- handleFormUploads defaultUploadPolicy
                                         defaultFileUploadPolicy
                                         (const store)
    sequence $ (f . formFileValue) <$> files

withErr :: String -> Handler b v ()
withErr msg = do 
  modifyResponse $ setResponseCode 500
  asJSON $ object ["message" .= msg]

allowVueDev :: Handler b v ()
allowVueDev = modifyResponse $ setHeader "Access-Control-Allow-Origin" "http://localhost:8080"

asJSON :: (ToJSON a, MonadSnap m) => a -> m ()
asJSON x = do 
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ x

main = serveSnaplet defaultConfig appInit
