{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Data.Monoid
import Control.Monad
import qualified Data.List as DL
import Data.Data
import Data.Time

type Amount = Double
type Rate = Int

data BudgetType = 
    Income 
  | Expense String
  deriving (Eq,Ord,Show,Read,Data,Typeable)

data BudgetItem = BudgetItem 
  { amount :: Amount
  , rate :: Rate
  , budgetType :: BudgetType
  } deriving (Eq,Ord,Show,Read,Data,Typeable)

type Budget = [BudgetItem]

data Bank = Bank
  { checking :: Amount
  , savings :: Amount
  , lastModified :: Rate
  } deriving (Eq,Ord,Show,Read,Data,Typeable)

toExpensesOnly :: Budget -> Budget
toExpensesOnly = filter (noIncome . budgetType)
  where
    noIncome Income = False
    noIncome (Expense _) = True

getExpense :: String -> Budget -> Maybe BudgetItem
getExpense n = DL.find (isName . budgetType)
  where
    isName Income = False
    isName (Expense t) = n == t

payOffAfter :: String -> Amount -> Rate -> Budget -> Maybe Amount
payOffAfter ename a r b = do
  e <- getExpense ename b
  return $ getBalanceAtPeriod a r [e]

-- runs a budget for "p" periods given a starting amount "start" and returns the
-- final balance. 
getBalanceAtPeriod :: Amount -> Rate -> Budget -> Amount
getBalanceAtPeriod start p = (start +) . getSum . mconcat . fmap 
  (\i -> Sum $ (amount i)*(fromIntegral . floor . toRational $ p `div` (rate i)))

printBalances :: Amount -> Rate -> Budget -> IO ()
printBalances start ds budget = sequence_ $ [printBal d | d <- [0..ds]]
  where
    printBal d = putStrLn $ 
         (show d)
      ++ ": "
      ++ (show $ getBalanceAtPeriod start d budget)

getEmptyDate :: Double -> Budget -> Int
getEmptyDate start budget = g 0
    where
      g n
        | (f n) <= 0 = n
        | otherwise = g (n+1)
      f n = getBalanceAtPeriod start (n+1) budget

makeExpense :: String -> Amount -> Rate -> BudgetItem
makeExpense t a r = BudgetItem 
  { amount = (-1)*(abs a)
  , rate = r
  , budgetType = Expense t
  }

makeIncome :: Amount -> Rate -> BudgetItem
makeIncome a r = BudgetItem
  { amount = abs a
  , rate = r
  , budgetType = Income
  }

startDate = fromGregorian 2017 09 01

income = 1730.77 :: Amount
loanAmount = 11352.14 :: Amount

bank = Bank
  { checking = 3868.88
  , savings = 1848.55
  , lastModified = 0
  }

budgetLoan = [ makeExpense "loan" (income - (income*0.1 + income*0.2)) 15 ]

budgetLiving =
  [ makeIncome 2000 500
  , makeIncome (income*0.1 + income*0.2) 15
  , makeExpense "tithe" (income*0.1) 15
  , makeExpense "taxes" (income*0.2) 15
  , makeExpense "rent" 350 31
  , makeExpense "auto" 20 7
  , makeExpense "food" 20 7
  , makeExpense "insurance" 76 31
  , makeExpense "phone" 30 31 
  , makeExpense "climbing" 55 31
  , makeExpense "mentoring" 7 31
  ]

main :: IO ()
main = do 
  putStrLn $ "Loan can be payed off in: " ++ (show n) ++ " days"
  putStrLn $ "Your living budget will last: " ++ (show m) ++ " days"
  putStrLn $ "The amount in your bank will be: " ++ (show x)
  {-putStrLn $ "You can pay off " ++ (show x) ++ " of your loan"-}
  {-putStrLn $ "Loan Budget lasts " ++ (show $ getBalanceAtPeriod loanAmount 122 budgetLoan) ++ " days"-}
  {-printBalances loanAmount 122 budgetLoan-}
  {-putStrLn $ "Diff: "  ++ (show x)-}
  where
    n = getEmptyDate loanAmount budgetLoan
    m = getEmptyDate (checking bank) budgetLiving
    x = getBalanceAtPeriod (checking bank) n budgetLiving
