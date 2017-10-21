{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : Name
Description : CSV file serialization
Copyright   : 
License     : GPL-2
Maintainer  : theNerd247
Stability   : experimental
Portability : POSIX

-}

module CSV
(
  saveCSVFile
  ,loadCSVFile
  ,parseDate
  ,CSVParseException(..)
)
where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Time
import Data.Data
import qualified Data.Csv as CSV
import qualified GHC.Exts as GE (toList)
import qualified Data.ByteString.Char8 as BS (null)
import Control.Applicative ((<|>))

import Data.ByteString.Lazy.Char8 (unpack,pack)
import Data.Char (isSpace)
import Data.Csv ((.!))
import Data.Vector ((!?))

-- | A parsing exception that contains the origin parser error
data CSVParseException = CSVParseException String deriving (Show,Typeable)

instance Exception CSVParseException where
  displayException (CSVParseException s) = s

instance CSV.ToField Day where
  toField = CSV.toField . formatTime defaultTimeLocale "%x"

instance CSV.FromField Day where
  parseField f = (maybe mempty return . parseDate) =<< (CSV.parseField f :: CSV.Parser String)

-- | Our custom CSV options 
csvEncodeOptions = CSV.defaultEncodeOptions 
  {
  -- dont quote anything
  CSV.encQuoting = CSV.QuoteNone
  }

dayFormats = ["%F"]

parseDate :: String -> Maybe Day
parseDate s = case parseTimes s of
  [] -> Nothing
  (x:xs) -> return x
  where
    parseTimes = parseFormat "%F"
    parseFormat f s = [t | (t,r) <- readSTime True defaultTimeLocale f s, all isSpace r]

-- | saves a csv compatable type to a file
saveCSVFile :: (MonadIO m, CSV.ToRecord a) => FilePath -> [a] -> m ()
saveCSVFile f = liftIO . writeFile f . unpack . CSV.encodeWith csvEncodeOptions

-- | loads a headerless csv file
loadCSVFile :: (MonadThrow m, MonadIO m, CSV.FromRecord a) => FilePath -> m [a]
loadCSVFile f = do 
  bs <- liftIO $ readFile f
  either mkerror mklist . CSV.decode CSV.HasHeader . pack $ bs
    where 
    -- | convert the result from a Vector to a []
    mklist = return . GE.toList
    -- | or convert to a SomeException
    mkerror s = throwM . CSVParseException $ s
      {-"Parser error in file: " ++ f ++ "\n" ++ " on line containing: " ++ s -- (lines s !! 1)-}
