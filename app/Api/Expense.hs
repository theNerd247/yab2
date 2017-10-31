{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Api.Expense (resource) where

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

type Identifier = Name

type WithExpense = ReaderT Identifier YabApi

resource :: Resource YabApi WithExpense Identifier () Void
resource = mkResourceReader
  { R.name = "expenses"
  , R.schema = withListing () $ named [ ("name", singleBy id)] 
  , R.list = const list
  , R.get = Just get
  , R.create = Just create
  , R.update = Just update
  }

get :: Handler WithExpense
get = mkIdHandler jsonO handler
  where
    handler :: () -> Identifier -> ExceptT Reason_ WithExpense ExpenseList
    handler _ name = do 
      db <- (lift . lift) (asks $ view db)
      (asYabList db name $ getExpensesByName db name) !? NotFound

list :: ListHandler YabApi
list = mkListing jsonO $ \range -> do
  db <- (asks $ view db)
  names <- getAllBudgetNames db
  forM names $ \name -> do
    is <- asYabList db name $ getExpensesByName db name
    return $ is & lifted.items %~ (take (count range) . drop (offset range))

create :: Handler YabApi
create = mkInputHandler (jsonI . jsonO) handler
  where
    handler :: ExpenseList -> ExceptT Reason_ YabApi ExpenseList
    handler newList = do
      db <- (lift) (asks $ view db)
      be <- getExpensesByName db (newList^.name)
      case (Data.IxSet.null be) of
        False -> throwE NotAllowed
        _ -> do 
          now <- liftIO $ getCurrentTime
          si <- liftIO $ setNewBID (newList^.startInfo)
          is <- liftIO $ forM (newList^.items) setNewBID
          let newList' = newList & startInfo .~ si & items .~ is
          insertExpenseList db newList'
          return newList'
          
update :: Handler WithExpense
update = mkInputHandler (jsonI . jsonO) handler
  where
    handler :: ExpenseList -> ExceptT Reason_ WithExpense ()
    handler bdata = do
      db <- (lift . lift) (asks $ view db)
      updateExpenseList db bdata
