{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Budget.Internal where

import Control.Lens hiding ((.=),Indexable)
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class
import Control.Monad.State (get,put,modify)
import Control.Monad.State.Class
import Data.Acid
import Data.Aeson hiding ((.~))
import Data.Audit
import Data.BID
import Data.Budget.InternalMigration
import Data.Data
import Data.Decimal
import Data.Default
import Data.Default.Time
import Data.IxSet
import Data.JSON.Schema hiding (Proxy, Object)
import Data.Monoid
import Data.SafeCopy
import Data.Time
import GHC.Generics hiding (to)
import qualified Data.List as DL
import qualified Data.Text as DT

type Amount = Decimal

amountToDouble :: Amount -> Double
amountToDouble (Decimal p m) = (fromInteger m) / (fromInteger $ 10 ^ p)

doubleToAmount :: Double -> Amount
doubleToAmount = realFracToDecimal 2

type Period = Int

data Rate = Periodic Period | OneTime UTCTime
  deriving (Eq,Ord,Read,Show,Data,Typeable,Generic)

type Name = String

type YabDB = IxSet 

type StartInfoDB = YabDB StartInfo

type StartInfoAuditDB = AuditDB StartInfo

newtype AmountType = AmountType String
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data BudgetAmount = BudgetAmount
  { _amount :: Amount
  , _amountType :: AmountType
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data StartInfo = StartInfo
  { _startInfoBName :: Name
  , _startDate :: UTCTime
  , _startAmount :: Amount
  , _startInfoBID :: BID
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data YabList a = YabList
  { _yabListStartInfo :: StartInfo
  , _items :: [a]
  } deriving (Eq,Ord, Show,Read,Data,Typeable,Generic)

makeClassy ''StartInfo

makeClassy ''Rate

makeClassy ''BudgetAmount

instance Migrate Rate where
  type MigrateFrom Rate = Rate_v0
  migrate = Periodic

instance Migrate Amount where
  type MigrateFrom Amount = Amount_v0
  migrate = doubleToAmount

instance SafeCopy Amount where
    putCopy a = contain $ do
      safePut $ decimalPlaces a
      safePut $ decimalMantissa a
    getCopy = contain $ Decimal <$> safeGet <*> safeGet
    version = 2
    kind = extension

$(deriveSafeCopy 1 'extension ''Rate)

$(deriveSafeCopy 0 'base ''AmountType)

$(deriveSafeCopy 0 'base ''BudgetAmount)

$(deriveSafeCopy 0 'base ''StartInfo)

$(deriveSafeCopy 0 'base ''YabList)

class HasName m where
  name :: Lens' m Name

class HasYabDB m a | m -> a where
  yabDB :: Lens' m (YabDB a)

class HasYabList m a | m -> a where
  yabList :: Lens' m (YabList a)

  items :: Lens' m [a]
  items = yabList . items

  yabListStartInfo :: Lens' m StartInfo
  yabListStartInfo = yabList . yabListStartInfo

instance Default Rate where
  def = Periodic 0

instance Default Amount where
  def = 0

instance Default BudgetAmount

instance Default AmountType

instance Default StartInfo

instance (Default a) => Default (YabList a) 

instance FromJSON Amount where
  parseJSON = fmap doubleToAmount . parseJSON

instance FromJSON AmountType where
  parseJSON = fmap AmountType . parseJSON

instance FromJSON BudgetAmount where
  parseJSON (Object o) = BudgetAmount
    <$> o .: "amount"
    <*> o .: "type"
  parseJSON _ = mempty

instance FromJSON Rate

instance (FromJSON a) => FromJSON (YabList a) where
  parseJSON v@(Object o) = YabList
    <$> o .: "startInfo"
    <*> o .: "items"
  parseJSON _ = mempty

instance (FromJSON a, Indexable a, Ord a, Typeable a) => FromJSON (YabDB a) where
  parseJSON = fmap fromList . parseJSON

instance FromJSON StartInfo where
  parseJSON (Object o) = StartInfo
    <$> o .: "name"
    <*> o .: "startDate"
    <*> o .: "startAmount"
    <*> o .: "id"
  parseJSON _ = mempty

instance Functor YabList where
  fmap f l = YabList
    { _yabListStartInfo = _yabListStartInfo l
    , _items = fmap f (_items l)
    }

instance HasBID StartInfo where
  bid = startInfoBID

instance HasName StartInfo where
  name = startInfoBName

instance HasName (YabList a) where
  name = yabList . startInfoBName

instance HasYabDB (YabDB a) a where
  yabDB = id

instance HasYabList (YabList a) a where
  yabList = id
  items = yabList . go where go f (YabList s l) = (\l' -> YabList s l') <$> f l
  yabListStartInfo = yabList . go where go f (YabList s l) = (\s' -> YabList s' l) <$> f s

instance HasStartInfo (YabList a) where
  startInfo = yabListStartInfo

instance JSONSchema Amount where
  schema _ = Data.JSON.Schema.Number $ Bound Nothing Nothing

instance JSONSchema AmountType where
  schema = gSchema

instance JSONSchema BudgetAmount where
  schema = gSchema

instance JSONSchema StartInfo where
  schema = gSchema

instance JSONSchema Rate where
  schema = gSchema

instance (JSONSchema a) => JSONSchema (YabList a) where
  schema = gSchema

instance (ToJSON a, Indexable a, Ord a, Typeable a) => ToJSON (YabDB a) where
  toJSON = toJSON . toList

instance ToJSON Amount where
  toJSON = toJSON . amountToDouble 

instance Indexable StartInfo where
  empty = ixSet 
    [ ixFun $ (:[]) . (view bid) 
    , ixFun $ (:[]) . (view name)
    , ixFun $ (:[]) . (view startDate)
    , ixFun $ (:[]) . (view startAmount)
    ]

instance ToJSON BudgetAmount where
  toJSON = object . budgetAmountJSON

instance ToJSON AmountType where
  toJSON (AmountType s) = toJSON s

instance ToJSON Rate

instance ToJSON StartInfo where
  toJSON = object . budgetStartJSON

instance (ToJSON a, HasBudgetAmount a) => ToJSON (YabList a) where 
  toJSON b = object $
    [ "items" .= toJSON (b^.items)
    , "startInfo" .= toJSON (b^.startInfo)
    ]

budgetAmountJSON a =
    ["amount" .= (a^.amount)
    ,"type" .= (a^.amountType)
    ]

budgetStartJSON b =
    ["name" .= (b^.name)
    ,"startDate" .= (b^.startDate)
    ,"startAmount" .= (b^.startAmount)
    ,"id" .= (b^.bid)
    ]

listToDB :: (Typeable a, Ord a, Indexable a) => YabList a -> YabDB a
listToDB = fromList . (view items)

{-# DEPRECATED dayToRate "Use dayToPeriod" #-}
dayToRate = dayToPeriod

{-# DEPRECATED rateToDay "Use dayToPeriod" #-}
rateToDay = periodToDay

dayToPeriod :: UTCTime -> UTCTime -> Period
dayToPeriod s = fromInteger . flip diffDays (utctDay s) . utctDay

rateToPeriod :: (HasStartInfo s) => s -> Rate -> Period
rateToPeriod _ (Periodic p) = p
rateToPeriod s (OneTime p) = dayToPeriod (s^.startDate) p

periodToDay :: UTCTime -> Period -> UTCTime
periodToDay s r = UTCTime (flip addDays (utctDay s) . toInteger $ r) 0

dayToDate :: Day -> UTCTime
dayToDate d = UTCTime d 0

budgetAmountAtPeriod :: (HasRate i, HasBudgetAmount i, HasStartInfo s) => s -> Period -> Getter i Amount
budgetAmountAtPeriod s p = to $ \b -> rt (b^.rate) b
  where
    rt (OneTime t) b = withO t b
    rt (Periodic r) b = withP r b
    withP r b
      | r == 0 = 0
      | p `mod` r == 0 = b^.amount
      | otherwise = 0
    withO t b
      | (dayToPeriod (s^.startDate) t) == p = b^.amount
      | otherwise = 0
      

-- runs a budget for "p" periods given a starting amount "start" and returns the
-- final balance. 
{-getBalanceAtPeriod :: (BudgetAtPeriod a, HasStartInfo (f a), Traversal' (f a) a) => Rate -> (f a) -> Amount-}
getBalanceAtPeriod p b = (b^.startAmount) + diff
  where
    diff 
      | p == 0 = 0
      | p > 0 = sum $ [1..p]^..traverse.to (\p -> b^.balanceDiff p)
      | p < 0 = ((-1)*) . sum $ [(p+1)..0]^..traverse.to (\p -> b^.balanceDiff p)

-- the sum of item amounts relative to the budget start amount and relative to
-- the start date
balanceDiff p = to $ \b -> sum $ b^..items.traverse.budgetAmountAtPeriod b p

-- gets the budget balance from start to end periods (both inclusive)
{-getBalancesBetween :: (BudgetAtPeriod a, HasStartInfo (f a), HasYabList (f a) a) => Rate -> Rate -> (f a) -> [Amount]-}
getBalancesBetween start end b = [start..end]^..traverse . to (flip getBalanceAtPeriod b)

earliestStartInfo :: [StartInfo] -> Maybe StartInfo
earliestStartInfo [] = Nothing
earliestStartInfo xs = Just . head $ DL.sortOn (view startDate) xs

-- returns the difference of the budget balance (b2 - b1) at each period between
-- the start and end times
compareBudgetsBetween start end b1 b2 = zip3 ([start..end]) (getBalancesBetween start end b1) (getBalancesBetween start end b2)

compareBudgetsOn p b1 b2 = ((getBalanceAtPeriod p b1), (getBalanceAtPeriod p b2))

-- get's the first period where the budget balance is <= 0
{-getEmptyDate :: (BudgetAtPeriod a, HasStartInfo (f a), HasYabList (f a) a) => f a -> Rate-}
{-getEmptyDate budget = g 0-}
    {-where-}
      {-g n-}
        {-| (f n) <= 0 = n-}
        {-| otherwise = g (n+1)-}
      {-f n = getBalanceAtPeriod (n+1) budget-}

getBalances s e b = [s..e]^..traverse . to (\d -> getBalanceAtPeriod d b)

-- prints the balance of the budget at each period from start to end
printBalances start end budget = sequence_ $ [start..end]^..traverse . to printBal
  where
    printBal d = putStrLn $ 
         (show d)
      ++ ": "
      ++ (show $ getBalanceAtPeriod d budget)

-- gets the budget balance for the current day (this uses utc time)
currentBudgetBal b = do 
  n <- getCurrentTime
  return $ getBalanceAtPeriod (dayToPeriod (b^.startDate) n) b

-- updates each element by the given key using updateIx
updateDBItems :: (HasYabDB r a, MonadState s m, Ord a, Indexable a, Typeable k, Typeable a) => Lens' s r -> k -> [a] -> m ()
updateDBItems l key values = l.yabDB %= (appEndo (foldMap (Endo . updateIx key) values))
