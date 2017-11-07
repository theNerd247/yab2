{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Api.BudgetList (resource, WithBudget) where

import Api.ApiTypes
import Control.Error.Util ((??),(!?))
import Control.Lens hiding ((??),(!?))
import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks,ask)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson
import Data.Budget
import Data.Data
import Data.IxSet
import Data.JSON.Schema hiding (Proxy)
import GHC.Generics
import Rest
import Rest.Dictionary
import Rest.Types.Info
import YabAcid
import qualified Data.Text as T
import qualified Rest.Resource as R

type WithBudget = ReaderT Name YabApi

type WithBudgetList a = ReaderT a WithBudget

data MID = All | AllNames | AllStatus

resource :: Resource YabApi WithBudget Name MID Void
resource = mkResourceReader
  { R.name = "budget-list"
  , R.schema = withListing All $ named [ ("name", singleBy id), ("names", listing AllNames), ("status", listing AllStatus)] 
  , R.list = list
  , R.get = Just get
  , R.create = Just create
  , R.update = Just update
  }

get :: Handler WithBudget
get = mkIdHandler jsonO handler
  where
    handler :: () -> Name -> ExceptT Reason_ WithBudget BudgetList
    handler _ name = do 
      db <- (lift . lift) (asks $ view db)
      bdb <- (asYabList db name $ getBudgetByName db name) !? NotAllowed
      return bdb 

list :: MID -> ListHandler YabApi
list All = listAll
list AllNames = listNames
list AllStatus = listAllStatuses

listAll :: ListHandler YabApi
listAll = mkListing jsonO $ \range -> do
  db <- (asks $ view db)
  names <- getAllBudgetNames db
  forM names $ \name -> do
    b <- asYabList db name $ getBudgetByName db name
    return $ b & lifted.items %~ (take (count range) . drop (offset range))

listAllStatuses :: ListHandler YabApi
listAllStatuses = mkListing jsonO $ \_ -> do
  db <- lift (ask $ view db)
  now <- liftIO $ getCurrentTime
  names <- getAllBudgetNames db
  forM names $ \nm -> do
    b <- (asYabList db nm $ getBudgetByName db nm) !? NotAllowed
    e <- (asYabList db nm $ getExpensesByName db nm) !? NotAllowed
    let c = compareBudgetsOn (dayToRate (b^.startDate) now) b e
    return $ (T.pack nm,c^._1,c^._2)

listNames :: ListHandler YabApi
listNames = mkListing jsonO $ \_ -> do
  db <- (asks $ view db)
  fmap T.pack <$> getAllBudgetNames db

create :: Handler YabApi
create = mkInputHandler (jsonI . jsonO) handler
  where
    handler :: BudgetList -> ExceptT Reason_ YabApi BudgetList
    handler newList = do
      db <- (lift) (asks $ view db)
      be <- getBudgetByName db (newList^.name)
      case (Data.IxSet.null be) of
        False -> throwE NotAllowed
        _ -> do 
          si <- liftIO $ setNewBID (newList^.startInfo)
          is <- liftIO $ forM (newList^.items) setNewBID
          let newList' = newList & startInfo .~ si & items .~ is
          insertBudgetList db newList'
          return newList'
          
update :: Handler WithBudget
update = mkInputHandler (jsonI . jsonO) handler
  where
    handler :: BudgetList -> ExceptT Reason_ WithBudget ()
    handler bdata = do
      db <- (lift . lift) (asks $ view db)
      updateBudgetList db bdata
