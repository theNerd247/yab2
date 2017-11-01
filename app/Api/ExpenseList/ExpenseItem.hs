module Api.ExpenseList.ExpenseItem where

import Control.Lens hiding ((??))
import Api.ExpenseList (WithExpenseList)
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

type Identifier = BID

type WithExpenseItem = ReaderT Identifier WithExpenseList

instance Info BID where
  describe _ = "bid"

resource :: Resource WithExpenseList WithExpenseItem Identifier Void Void
resource = mkResourceReader
  { R.name = "expense"
  , R.schema = noListing $ named [("id",singleBy BID)]
  , R.create = Just create
  , R.get = Just get
  }

get :: Handler WithExpenseItem
get = mkIdHandler jsonO $ \_ id -> do
  db <- (lift . lift . lift) (asks $ view db)
  nm <- (lift . lift) ask
  getOne <$> getExpensesByBID db id

create :: Handler WithExpenseList
create = mkInputHandler (jsonI . jsonO) handler
  where
    handler :: ExpenseItem -> ExceptT Reason_ WithExpenseList ExpenseItem
    handler newItem = do
      db <- (lift . lift) (asks $ view db)
      nm <- lift ask
      newItem' <- liftIO $ setNewBID newItem
      insertExpenseItem db $ newItem' & name .~ nm
      return $ newItem'
