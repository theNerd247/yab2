{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Api.ByRange where

import Rest.Types.Info
import GHC.Generics
import Data.Time
import Data.Data
import Data.Aeson
import Data.JSON.Schema hiding (Proxy)
import Data.JSON.Schema.Combinators (value)
import Rest
import Rest.Dictionary (withParam)

data DayRange = DayRange
  { sdate :: Day
  , edate :: Day
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

instance ToJSON DayRange
instance FromJSON DayRange

instance JSONSchema Day where
  schema _ = value

instance JSONSchema DayRange where
  schema = gSchema

dayRangeParam = mkPar . fmap Just $ DayRange <$> withParam "sdate" <*> withParam "edate"
