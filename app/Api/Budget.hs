{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Api.Budget where

import Control.Lens hiding ((??),(!?))
import Data.Budget
import YabAcid
import Data.Data
import GHC.Generics
import Data.IxSet
import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.JSON.Schema hiding (Proxy)
import Control.Error.Util ((??),(!?))
import Data.Aeson
import qualified Data.Text as T

import Api.ApiTypes
import Rest
import Rest.Types.Void
import qualified Rest.Resource as R

type WithBudget = ReaderT Name YabApi

instance JSONSchema AmountType where
  schema = gSchema

instance JSONSchema BudgetAmount where
  schema = gSchema

instance JSONSchema StartInfo where
  schema = gSchema

instance JSONSchema BudgetItem where
  schema = gSchema

instance (JSONSchema a) => JSONSchema (YabList a) where
  schema = gSchema

resource :: Resource YabApi WithBudget Name () Void
resource = mkResourceReader
  { R.name = "budget"
  , R.schema = withListing () $ named [ ("name", singleBy id) ] 
  , R.list = const list
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

list :: ListHandler YabApi
list = mkListing jsonO $ \range -> do
  db <- (asks $ view db)
  names <- getAllBudgets db
  forM names $ \(name,bs) -> do
    si <- (earliestStartInfo . toList <$> getStartInfoByName db name) !? NotAllowed
    return $ (def :: BudgetList)
      & items .~ bs
      & startInfo .~ si

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
          now <- liftIO $ getCurrentTime
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
