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
import qualified Simple.Aeson as SA
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

yabListSnapInit :: (HasYabAcidSnaplet b, Default a, FromJSON a, ToJSON a, HasYabAcid a) => SnapletInit b (YabListSnaplet a)
yabListSnapInit = makeSnaplet "yablist" "snaplet for yablist interface" Nothing $ do
  addRoutes [("", runAesonApi getListFromParams)
            ,("names", runAesonApi getNames)
            ,("size", getYabListSize)
            ,("update", update)
            ,("create", create)
            ]
  return Proxy

getNames :: (HasYabAcidSnaplet b) => Handler b (YabListSnaplet a) [Name]
getNames = do
  ns <- withYabSnapletDB $ uses (yabAcidLens :: YabAcidLens BudgetItem) groupBy
  return $ ns^..folded._1

withFilter :: Handler b v YabListFilter
withFilter = YabListFilter 
  <$> (skipParse <$> fromParam "name") 
  <*> (fmap skipParse <$> fromParams ' ' "subNames")

getYabDB :: (HasYabAcidSnaplet b, HasYabAcid a) => Handler b (YabListSnaplet a) (IxSet a)
getYabDB = do
  filter <- withFilter
  withYabSnapletDB $ getItemsBy (@= (filter^.nameFilter))

getYabList :: (HasYabAcidSnaplet b, Default a, HasYabAcid a) => Name -> Handler b (YabListSnaplet a) (YabList a)
getYabList nm = throwMaybe (AsYabListError "Couldn't convert to yablist in get") =<< (withYabSnapletDB $ asYabList nm =<< getItemsBy (@= nm))

getListFromParams :: (HasYabAcidSnaplet b, Default a, HasYabAcid a, ToJSON a) => Handler b (YabListSnaplet a) (YabList a)
getListFromParams = do
  filter <- withFilter
  ylist <- getYabList $ filter^.nameFilter
  return $ filterBudgetItems (filter^.subNamesFilter) ylist

withCount :: (HasItems a) => a -> Handler b v a
withCount xs = do
  offset <- fromParam "offset"
  count <- fromParam "count"
  return $ xs & items %~ take count . drop offset 

getFromBody :: (FromJSON a, HasYabAcid a) => Handler b (YabListSnaplet a) (YabList a)
getFromBody = SA.fromBody

getYabListSize :: (HasYabAcidSnaplet b, HasYabAcid a) => Handler b (YabListSnaplet a) ()
getYabListSize = runAesonApi $ size <$> getYabDB

update :: (HasYabAcidSnaplet b, HasYabAcid a, FromJSON a, ToJSON a, Default a) => Handler b (YabListSnaplet a) ()
update = runAesonApi $ withYabSnapletDB . updateYabList =<< getFromBody

create :: (HasYabAcidSnaplet b, HasYabAcid a, FromJSON a, ToJSON a, Default a) => Handler b (YabListSnaplet a) ()
create = runAesonApi $ withYabSnapletDB . insertYabList =<< getFromBody

delete :: (HasYabAcidSnaplet b, HasYabAcid a, Default a, ToJSON a) => Handler b (YabListSnaplet a) ()
delete = runAesonApi $ withYabSnapletDB . deleteYabList =<< getListFromParams

getAll :: (HasYabAcidSnaplet b, HasYabAcid a, Default a, ToJSON a) => Handler b (YabListSnaplet a) ()
getAll = runAesonApi $ do
  ns <- getNames
  forM ns $ \nm -> getYabList nm

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
    {-return $ b & lifted.items %~ -}

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
