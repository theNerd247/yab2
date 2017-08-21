module Main where

import Data.Monoid
import Control.Monad

type Amount = Double
type Rate = Int

data BudgetType = 
    Income 
  | Expense String
  deriving (Eq,Ord,Show,Read)

data BudgetItem = BudgetItem 
  { amount :: Amount
  , rate :: Rate
  , budgetType :: BudgetType
  } deriving (Eq,Ord,Show,Read)

type Budget = [BudgetItem]

data Bank = Bank
  { checking :: Amount
  , savings :: Amount
  , lastModified :: Rate
  } deriving (Eq,Ord,Show,Read)

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
  { amount = -1*a
  , rate = r
  , budgetType = Expense t
  }

makeIncome :: Amount -> Rate -> BudgetItem
makeIncome a r = BudgetItem
  { amount = a
  , rate = r
  , budgetType = Income
  }

income = 1730.77 :: Amount
loanAmount = 11352.14 :: Amount

bank = Bank
  { checking = 4098.02
  , savings = 1848.55
  , lastModified = 0
  }

budgetA = 
  [ makeIncome income 14
  , makeExpense "tithe" (income*0.1) 14
  , makeExpense "tax" (income*0.2) 14 
  , makeExpense "loan" (income - (income*0.1 + income*0.2)) 14
  ]

budgetB =
  [ makeIncome 2000 500
  , makeExpense "rent" 350 31
  , makeExpense "auto" 20 7
  , makeExpense "food" 20 7
  , makeExpense "insurance" 76 31
  , makeExpense "phone" 30 31 
  ]

main :: IO ()
main = do 
  putStrLn $ "Budget lasts " ++ (show n) ++ " days"
  putStrLn $ "Loan Budget lasts " ++ (show $ getBalanceAtPeriod loanAmount 122 budgetA) ++ " days"
  printBalances loanAmount 122 budgetA
  putStrLn $ "Diff: "  ++ (show x)
  where
    x = (checking bank) - (getBalanceAtPeriod (checking bank) 122 budgetB)
    n = getEmptyDate (checking bank) budgetB
    m = getEmptyDate loanAmount budgetA
