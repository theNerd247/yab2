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
import Data.Time (Day)
import Data.Traversable (forM)
import Snap
import Snap.Snaplet.Heist
import Snap.Util.FileUploads
import System.Directory
import System.FilePath
import Snap.Snaplet.Router
import Snap.Snaplet.Router.Types
import Web.Routes.PathInfo
import YabAcid
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

data App = App
  { _db :: YabAcidState
  , _router :: Snaplet RouterState
  } 

makeLenses ''App

data AppUrl = 
  AppExpensesUrl ExpensesUrl
  deriving (Eq,Ord,Show,Read,Generic)

data ExpensesUrl =
  ByDate Day Day
  | ByName Name
  | UploadTransaction Name
  deriving (Eq,Ord,Show,Read,Generic)

instance PathInfo ExpensesUrl

instance PathInfo AppUrl where
  toPathSegments (AppExpensesUrl e) = "expenses" : toPathSegments e
  fromPathSegments = segment "expenses" *> (AppExpensesUrl <$> fromPathSegments)

instance PathInfo Day where
  toPathSegments d = [T.pack $ show d]
  fromPathSegments = pToken (const "parse date") $ parseDate . T.unpack
  
instance HasRouter (Handler App App) where
  type URL (Handler App App) = AppUrl
  getRouterState = with router get

instance HasRouter (Handler b RouterState) where
  type URL (Handler b RouterState) = AppUrl
  getRouterState = get

appInit :: SnapletInit App App 
appInit = makeSnaplet "myapp" "Yab snaplet" Nothing $ do
  dbRef <- liftIO $ openLocalStateFrom "/tmp/tst" (def :: YabAcid)
  r <- nestSnaplet "router" router $ initRouter ""
  addRoutes [("", routeWith routeAppUrl),("/tstroutes",tstroutes)]
  onUnload $ closeAcidState dbRef
  return $ App dbRef r

tstroutes = do
  p <- urlPath $ AppExpensesUrl $ ByName "tst"
  asJSON $ p

routeAppUrl :: AppUrl -> Handler App App ()
routeAppUrl (AppExpensesUrl e) = routeExpensesUrl e

routeExpensesUrl :: ExpensesUrl -> Handler App App ()
routeExpensesUrl (ByDate sdate edate) = method GET $ do
  db <- asks $ view db
  getExpensesByDate db sdate edate >>= asJSON . (sortOn $ view expenseDate) . toList

routeExpensesUrl (ByName name) = method GET $ do
  db <- asks $ view db
  getExpensesByName db name >>= asJSON . toList

routeExpensesUrl (UploadTransaction name) = do
  dups <- uploadCSVFiles $ \f -> do
    liftIO . putStrLn $ "uploaded: " ++ f
    db <- asks $ view db
    es <- loadNewTransactionFile name f
    mergeExpenses db es
  asJSON $ mconcat dups

allowVueDev :: Handler b v ()
allowVueDev = modifyResponse $ setHeader "Access-Control-Allow-Origin" "http://localhost:8080"

asJSON :: (ToJSON a) => a -> Handler b v ()
asJSON x = do 
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ x

uploadCSVFiles :: (FilePath -> Handler b App a) -> Handler b App [a]
uploadCSVFiles f = withTemporaryStore "/tmp" "yab-" $ \store -> do
    (inputs, files) <- handleFormUploads defaultUploadPolicy
                                         defaultFileUploadPolicy
                                         (const store)
    sequence $ (f . formFileValue) <$> files

main = serveSnaplet defaultConfig appInit
