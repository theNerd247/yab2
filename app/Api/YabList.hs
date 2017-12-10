{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.YabList where

import           Control.Lens hiding (Indexable)
import           Control.Monad              (forM)
import           Control.Monad.Reader       (MonadReader,ReaderT(..),ask,asks)
import           Control.Monad.Trans        (MonadIO,lift,liftIO)
import           Control.Monad.Catch hiding (Handler)
import           Control.Monad.State        (gets)
import           Data.Aeson
import           Data.Budget
import           Data.Data
import           Data.IxSet          hiding (Proxy)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           GHC.Generics
import           YabAcid
import Simple hiding (fromBody,fromParam,fromHeader)
import Simple.Aeson (runAesonApi)
import Simple.Snap
import Simple.String
import Snap
import Api.ApiTypes
import Data.Proxy

data YabListFilter = YabListFilter
  { _nameFilter :: Name
  , _subNamesFilter :: [Name]
  } deriving (Show,Read,Ord,Eq,Data,Typeable,Generic)

makeLenses ''YabListFilter

type YabListSnaplet a = Proxy a

yabListSnapInit :: (HasYabAcidSnaplet b, Default a, ToJSON a, HasYabAcid a, HasBudgetAmount a) => SnapletInit b (YabListSnaplet a)
yabListSnapInit = makeSnaplet "yablist" "snaplet for yablist interface" Nothing $ do
  addRoutes [("", runAesonApi getYabList)]
  return Proxy

getYabList :: (HasYabAcidSnaplet b, Default a, HasYabAcid a, ToJSON a) => Handler b (YabListSnaplet a) (YabList a)
getYabList = do
  filter <- YabListFilter <$> fromParam "name" <*> fromParam "subNames"
  let nm = filter^.nameFilter
  db <- withYabSnapletDB $ asYabList nm =<< getItemsBy (@= nm)
  ylist <- throwMaybe (AsYabListError "Couldn't convert to yablist in get") db
  return $ filterBudgetItems (filter^.subNamesFilter) ylist

{-class (HasYabAcid a, Default a, Indexable a) => HasYabListApiResource a where-}
  {-getHandler :: ExceptT Reason_ WithYabList (YabList a)-}
  {-getHandler = do-}
    {-name <- ask-}

    {-db ?? NotAllowed-}

  {-getSizeHandler :: ExceptT Reason_ WithYabList (Tagged a Int)-}
  {-getSizeHandler = ask >>= \name -> fmap Tagged . withYABDB db $ itemCount =<< getItemsBy (@= name)-}

  {-updateHandler :: YabList a -> ExceptT Reason_ WithYabList (YabList a)-}
  {-updateHandler = withYABDB db . updateYabList-}

  {-resource :: Tagged a String -> Resource YabApi WithYabList Name MID Void-}
  {-resource (Tagged name) = mkResourceReader-}
    {-{ R.name    = name-}
    {-, R.schema  = withListing All $ named -}
        {-[ ("name"   , singleBy id) -}
        {-, ("names"  , listing AllNames)-}
        {-, ("status" , listing AllStatus)-}
        {-]-}
    {-{-, R.list    = list-}-}
    {-, R.get     = Just get-}
    {-{-, R.create  = Just create-}-}
    {-{-, R.update  = Just update-}-}
    {-{-, R.selects = [("size", getSize)]-}-}
    {-}-}
      {-where-}
        {-get = mkConstHandler jsonO getHandler-}

{-createHandler :: YabList a -> ExceptT Reason_ WithYabList (YabList a)-}
{-createHandler = withYABDB db . insertYabList-}

{-instance HasYabListApiResource BudgetItem-}

{-instance HasYabListApiResource ExpenseItem-}

{-getSize :: Handler WithYabList-}
{-getSize = mkIdHandler jsonO $ \_ name -> do-}
  {-bl <- lift . lift $ withYABDB db $ getItemsBy (@= name)-}
  {-return $ size bl-}

{-list :: MID -> ListHandler YabApi-}
{-list All       = listAll-}
{-list AllNames  = listNames-}
{-list AllStatus = listAllStatuses-}
{-list GetAudit  = listAudit-}

{-listAll :: ListHandler YabApi-}
{-listAll = mkListing jsonO $ \range -> do-}
  {-names <- withYABDB db $ asks (view budgetDB) >>= return . fmap name-}
  {-forM names $ \name -> do-}
    {-b <- withYABDB db $ asYabList name $ getItemsBy (@= name)-}
    {-return $ b & lifted.items %~ (take (count range) . drop (offset range))-}

{-listAllStatuses :: ListHandler YabApi-}
{-listAllStatuses = mkListing jsonO $ \_ -> do-}
  {-db <- lift (asks $ view db)-}
  {-now <- liftIO getCurrentTime-}
  {-names <- getAllBudgetNames db-}
  {-forM names $ \nm -> do-}
    {-b <- asYabList db nm (getBudgetByName db nm)   !? NotAllowed-}
    {-e <- asYabList db nm (getExpensesByName db nm) !? NotAllowed-}
    {-let c = compareBudgetsOn (dayToRate (b^.startDate) now) b e-}
    {-return (T.pack nm,c^._1,c^._2)-}

{-listAudit :: ListHandler YabApi-}
{-listAudit = mkListing jsonO $ \_ -> do-}
  {-db <- lift (asks $ view db)-}
  {-toList <$> getBudgetAuditDB db-}

{-listNames :: ListHandler YabApi-}
{-listNames = mkListing jsonO $ \_ -> do-}
  {-db <- asks $ view db-}
  {-fmap T.pack <$> getAllBudgetNames db-}
