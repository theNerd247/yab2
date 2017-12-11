{-# LANGUAGE FlexibleContexts #-}

module Data.Budget.Internal where

import Control.Lens
import Data.Time
import Data.Budget.Amount
import Data.Budget.Items
import Data.Budget.Name
import Data.Budget.Rate
import Data.Budget.StartInfo
import qualified Data.List as DL

budgetAmountAtPeriod s p = to $ \b -> rt (b^.rate) b
  where
    rt (OneTime t) b = withO t b
    rt (Periodic r) b = withP r b
    withP r b
      | r == 0 = 0
      | p `mod` r == 0 = b^.amount
      | otherwise = 0
    withO t b
      | (t^.from (periodDateIso $ s^.startDate)) == p = b^.amount
      | otherwise = 0

-- runs a budget for "p" periods given a starting amount "start" and returns the
-- final balance. 
getBalanceAtPeriod p = to $ \b -> (b^.startAmount) + (b^.diff p)

diff p
  | p == 0 = to $ const 0
  | p > 0 = to $ \b -> sum $ [1..p]^..traverse.to (\p -> b^.balanceDiff p)
  | p < 0 = to $ \b -> ((-1)*) . sum $ [(p+1)..0]^..traverse.to (\p -> b^.balanceDiff p)

-- the sum of item amounts relative to the budget start amount and relative to
-- the start date
balanceDiff p = to $ \b -> sum $ b^..items.traverse.budgetAmountAtPeriod b p

-- gets the budget balance from start to end periods (both inclusive)
getBalancesBetween start end b = [start..end]^..traverse . to (flip getBalanceAtPeriod b)

earliestStartInfo :: [StartInfo] -> Maybe StartInfo
earliestStartInfo [] = Nothing
earliestStartInfo xs = Just . head $ DL.sortOn (view startDate) xs

-- returns the difference of the budget balance (b2 - b1) at each period between
-- the start and end times
compareBudgetsBetween start end b1 b2 = zip3 ([start..end]) (getBalancesBetween start end b1) (getBalancesBetween start end b2)

compareBudgetsOn p b1 b2 = ((getBalanceAtPeriod p b1), (getBalanceAtPeriod p b2))

getBalances s e b = [s..e]^..traverse . to (\d -> getBalanceAtPeriod d b)

-- gets the budget balance for the current day (this uses utc time)
currentBudgetBal b = do 
  n <- getCurrentTime
  return $ b^.getBalanceAtPeriod (n^.from (periodDateIso $ b^.startDate))

-- | filters a yablist by the given whitelist of items to search for. If the
-- whitelist is empty the original yablist is returned
filterBudgetItems [] b = b
filterBudgetItems bs b = b & items %~ DL.filter hasName
  where
    ns = view name <$> bs
    hasName x = x^.name `DL.elem` ns
