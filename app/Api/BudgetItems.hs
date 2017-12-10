{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Api.BudgetItems (resource, WithYabList) where

import           Api.ApiTypes
import           Control.Error.Util         ((!?), (??))
import           Control.Lens               hiding ((!?), (??))
import           Control.Monad              (forM)
import           Control.Monad.Reader       (MonadReader, ReaderT (..), ask,
                                             asks)
import           Control.Monad.Trans        (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Data.Aeson
import           Data.Budget
import           Data.Data
import           Data.IxSet
import           Data.JSON.Schema           hiding (Proxy)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           GHC.Generics
import           Rest
import           Rest.Dictionary
import qualified Rest.Resource              as R
import           Rest.Types.Info
import           YabAcid


