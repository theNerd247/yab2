{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module Data.Budget.Rate where
  
import Data.Aeson
import Control.Lens
import Data.Budget.Items
import Data.Time
import Data.Data
import GHC.Generics hiding (from)
import Data.Budget.StartInfo
import Data.Default
import Data.SafeCopy
import Data.Budget.StartInfo
import Control.Applicative ((<**>))

type Period = Int

data Rate = Periodic Period | OneTime UTCTime
  deriving (Eq,Ord,Read,Show,Data,Typeable,Generic)

makeClassy ''Rate

type Rate_v0 = Int

instance Migrate Rate where
  type MigrateFrom Rate = Rate_v0
  migrate = Periodic

$(deriveSafeCopy 1 'extension ''Rate)

instance Default Rate where
  def = Periodic 0

instance FromJSON Rate where
  parseJSON v@(String _) = OneTime <$> (parseJSON v)
  parseJSON v@(Number _) = Periodic <$> (parseJSON v)
  parseJSON _ = mempty

instance ToJSON Rate where
  toJSON (Periodic p) = toJSON p
  toJSON (OneTime t) = toJSON t

rateDate :: UTCTime -> Iso' Rate UTCTime
rateDate s = ratePeriodIso s . periodDateIso s

periodDateIso :: UTCTime -> Iso' Period UTCTime
periodDateIso s = iso set get
  where
    set r = UTCTime (flip addDays (utctDay s) . toInteger $ r) 0
    get = fromInteger . flip diffDays (utctDay s) . utctDay

ratePeriodIso :: UTCTime -> Iso' Rate Period
ratePeriodIso s = iso set get
  where
    set (Periodic p) = p
    set (OneTime p) = p^.from (periodDateIso s)
    get = Periodic

dayDateIso :: Iso' Day UTCTime
dayDateIso = iso (flip UTCTime 0) (utctDay) 
