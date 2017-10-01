module Main where

import Data.Acid
import Data.Yaml (encodeFile)
import Data.Budget
import Data.Default (def)
import Control.Lens
import Data.Time (utctDay, getCurrentTime, fromGregorian)
import System.Directory
import System.FilePath
import Data.Traversable (forM)

income = 1730.77 :: Amount
loanAmount = 11352.14 :: Amount

bank = def & checking .~ 3868.88 & savings .~ 1848.55

mkBItem e a r = (def :: BudgetItem)
    & budgetType .~ Expense e
    & rate .~ r 
    & amount .~ a

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

getFromDir d ext = listDirectory d >>= return . fmap (d </>). filter ((==ext) . takeExtension)

main = do
  -- open database
  db <- openLocalStateFrom "expenses-acid" ([] :: ExpensesDB)
  -- search for new transaction files
  newTransactionFiles <- getFromDir "transactions" ".csv"
  -- search for new expenses files
  newExpensesFiles <- getFromDir "transactions" ".yaml"
  -- convert and save the new transactions so we can add reasons
  forM newTransactionFiles $ \f -> do 
    loadNewTransactionFile "" f
    putStrLn $ "Expenses file ready! " ++ (f -<.> "yaml")
  -- upsert new expenses files
  forM newExpensesFiles $ \f -> do 
    e <- loadYamlFile f :: IO Expenses
    dups <- update db . UpsertExpenses $ e
    case dups of
      [] -> putStrLn $ f ++ " Successfully loaded!"
      _ -> do
        putStrLn $ "You have duplicate expenses in: " ++ f
        putStrLn $ "I've merged the unique entries for you...you'll find the duplicates in: " ++ dupsFP
        encodeFile dupsFP (e & items .~ dups)
        where
          dupsFP = ((f -<.> "") ++ "_dups.yaml")    
