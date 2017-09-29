module Main where

import Data.Budget
import Data.Default (def)
import Control.Lens
import Data.Time (fromGregorian)

budgetTest :: Budget
budgetTest = def
  & name .~ "Test"
  & startAmount .~ 10.00
  & items .~ 
    [mkItem 2 1 "camels"
    ,mkItem 7 4 "potatoes"
    ,mkItem 6 0.2 "beans"
    ]
  where
    mkItem r a e = def 
      & rate .~ r 
      & budgetType .~ Expense e
      & amount .~ a

expensesTest :: Expenses
expensesTest = def 
  & name .~ "Test"
  & startDate .~ sDate
  & startAmount .~ budgetTest^.startAmount
  & items .~ 
    [mkItem 7 1 "Food" "beans"
    ,mkItem 7 2 "Animals" "camels"
    ,mkItem 7 4 "goofy" "potatoes"
    ]
  where
    mkItem d a e r = def
      & expenseDate .~ rateToDay sDate d
      & expenseReason .~ r
      & budgetType .~ Expense e
      & amount .~ a
    sDate = fromGregorian 2017 09 01

main = do 
  putStrLn . show $ getBalancesBetween 0 7 budgetTest
  putStrLn . show $ getBalancesBetween 0 7 expensesTest 
  putStrLn . show $ compareBudgetsBetween 0 7 budgetTest expensesTest
