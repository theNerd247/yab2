{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Api.ByRange where

import Rest.Types.Info
import Data.Budget (Name)
import GHC.Generics
import Data.Time
import Data.Data
import Data.Aeson
import Data.JSON.Schema hiding (Proxy)
import Data.JSON.Schema.Combinators (value)
import qualified Data.Text as T
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

dayRangeParam = mkPar $ DayRange <$> withParam "sdate" <*> withParam "edate"

newtype BudgetItemFilter = BudgetItemFilter [T.Text] 
  deriving (Eq,Ord,Read,Show,Data,Typeable, Generic)

instance ToJSON BudgetItemFilter

instance FromJSON BudgetItemFilter

instance JSONSchema BudgetItemFilter where
    schema = gSchema

budgetItemFilter = mkPar $ BudgetItemFilter <$> withParam "budgetItems"
