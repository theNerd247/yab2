module Api.Upload where

import Data.Bank
import YabAcid
import Simple
import Simple.Snap
import Simple.Aeson
import Snap.Util.FileUploads
import Snap

uploadSnaplet :: SnapletInit YabSnaplet ()
uploadSnaplet = makeSnaplet "transaction" "uploads a new transaction" Nothing $ do
  addRoutes [(":name", uploadTransaction)]
  return ()

uploadTransaction :: Handler b App ()
uploadTransaction = runAesonApi $ do 
  name <- fromParam "name"
  uploadCSVFiles $ \f -> do
    es <- loadNewTransactionFile (B.unpack name) f
    withYabSnapletDB $ forM es updateItem >>= return . length

uploadCSVFiles :: (FilePath -> Handler b App a) -> Handler b App [a]
uploadCSVFiles f = withTemporaryStore "/tmp" "yab-" $ \store -> do
    (inputs, files) <- handleFormUploads defaultUploadPolicy
                                         defaultFileUploadPolicy
                                         (const store)
    sequence $ (f . formFileValue) <$> files
