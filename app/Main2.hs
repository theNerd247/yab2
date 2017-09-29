module Main where

import Data.Budget
import Data.Default (def)
import Control.Lens
import Data.Time (utctDay, getCurrentTime, fromGregorian)

currentDay = utctDay <$> getCurrentTime

income = 1730.77 :: Amount
loanAmount = 11352.14 :: Amount

bank = def & checking .~ 3868.88 & savings .~ 1848.55

mkBItem e a r = (def :: BudgetItem)
    & budgetType .~ Expense e
    & rate .~ r 
    & amount .~ a

currentBudgetBal :: Budget -> IO Amount
currentBudgetBal b = do 
  n <- currentDay 
  return $ getBalanceAtPeriod (dayToRate (b^.startDate) n) b

budgetLoan :: Budget
budgetLoan = def 
  & name .~ "Loan"
  & startDate .~ fromGregorian 2017 09 01
  & startAmount .~ loanAmount
  & items .~
    [mkBItem "loan" (income - (income*0.1 + income*0.2)) 15
    ]
  where

budgetLiving :: Budget
budgetLiving = def
  & name .~ "Living"
  & startDate .~ fromGregorian 2017 09 01
  & startAmount .~ income
  & items .~ 
    [ def & amount .~ 500 & rate .~ 2000
    , def & amount .~ (income*0.1 + income*0.2) & rate .~ 15
    , mkBItem "tithe" (income*0.1) 15
    , mkBItem "taxes"  (income*0.2) 15
    , mkBItem "rent"      350 31
    , mkBItem "auto"      20 7
    , mkBItem "food"      20 7
    , mkBItem "insurance" 76 31
    , mkBItem "phone"     30 31 
    , mkBItem "climbing"  55 31
    , mkBItem "mentoring" 7  31
    ]
    
main = currentBudgetBal budgetLoan >>= putStrLn . show 
