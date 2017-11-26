module Api.BudgetList.BudgetItem where

import Control.Lens hiding ((??))
import Api.BudgetList (WithBudgetList)
import Control.Error.Util ((??),(!?))
import Control.Monad.Reader (MonadReader, ReaderT (..), asks,ask)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Budget
import Data.IxSet
import Rest
import Api.ApiTypes
import YabAcid
import Rest.Dictionary.Types
import qualified Rest.Resource as R
import Rest.Types.Info
import Data.JSON.Schema

type Identifier = BID

type WithBudgetItem = ReaderT Identifier WithBudgetList

instance Info BID where
  describe _ = "bid"

-- resource :: Resource WithBudgetList WithBudgetItem Identifier Void Void
resource = mkResourceReader
  { R.name = "budget"
  , R.schema = noListing $ named [("id",singleBy BID)]
  , R.create = Just create
  , R.get = Just get
  , R.update = Just update
  , R.remove = Just remove
  }

get :: Handler WithBudgetItem
get = mkIdHandler jsonO $ \_ id -> do
  db <- (lift . lift . lift) (asks $ view db)
  nm <- (lift . lift) ask
  getOne <$> getBudgetByBID db id

create :: Handler WithBudgetList
create = mkInputHandler (jsonI . jsonO) handler
  where
    handler :: BudgetItem -> ExceptT Reason_ WithBudgetList BudgetItem
    handler newItem = do
      db <- (lift . lift) (asks $ view db)
      nm <- lift ask
      newItem' <- liftIO $ setNewBID newItem
      insertBudgetItem db $ newItem' & name .~ nm
      return newItem'

update :: Handler WithBudgetItem
update = mkInputHandler (jsonI . jsonO) $ \e -> do
  db <- (lift . lift . lift) (asks $ view db)
  updateBudgetItem db e

remove :: Handler WithBudgetItem
remove = mkIdHandler jsonO $ \_ id -> do
  db <- (lift . lift . lift) (asks $ view db)
  e <- (getOne <$> getBudgetByBID db id) !? NotAllowed
  deleteBudgetItem db e
