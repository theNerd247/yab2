{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Budget.Amount where

import Control.Lens
import Data.SafeCopy
import Data.Aeson
import Data.Default
import Data.Decimal

type Amount = Decimal

amountDoubleIso :: Iso' Amount Double
amountDoubleIso = iso set get
  where
    set (Decimal p m) = (fromInteger m) / (fromInteger $ 10 ^ p)
    get = realFracToDecimal 2

type Amount_v0 = Double

type AmountType_v0 = String

data BudgetAmount_v0 = BudgetAmount_v0
  { _budgetAmountAmountV0 :: Amount
  , _budgetAmountAmountTypeV0 :: AmountType_v0
  } deriving (Eq,Ord,Show,Read)

$(deriveSafeCopy 0 'base ''BudgetAmount_v0)

class HasAmount a where
  amount :: Lens' a Amount

instance HasAmount Amount where
  amount = id

instance Migrate Amount where
  type MigrateFrom Amount = Amount_v0
  migrate = view $ from amountDoubleIso

instance SafeCopy Amount where
    putCopy a = contain $ do
      safePut $ decimalPlaces a
      safePut $ decimalMantissa a
    getCopy = contain $ Decimal <$> safeGet <*> safeGet
    version = 2
    kind = extension

instance Default Amount where
  def = 0

instance ToJSON Amount where
  toJSON = toJSON . (view amountDoubleIso) 

instance FromJSON Amount where
  parseJSON = fmap (view $ from amountDoubleIso) . parseJSON
