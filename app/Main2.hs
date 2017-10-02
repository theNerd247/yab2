module Main where

import Data.Acid
import Data.Yaml (encodeFile)
import Data.Budget
import Control.Exception
import Control.Monad.Catch
import Data.Default (def)
import Control.Lens hiding ((<.>))
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

data NoNameException = NoNameException String
  deriving (Eq,Ord,Show,Read)

instance Exception NoNameException

guardNoName :: FilePath -> Expenses -> IO a -> IO a
guardNoName f e m
  | e^.name == "" = throwM . NoNameException $ "You need to name the expenses! Not merging: " ++ f
  | otherwise = m

prefixOf n = reverse . take n . reverse

backupFile :: FilePath -> IO ()
backupFile f = renameFile f (f <.> ".bak")

main = do
  -- open database
  db <- openLocalStateFrom "expenses-acid" ([] :: ExpensesDB)
  -- search for new transaction files
  newTransactionFiles <- getFromDir "transactions" ".csv"
  -- search for new expenses files
  newExpensesFiles <- getFromDir "transactions" ".yaml" >>= return . filter ((\x -> x/="merge.yaml" && x/="_dups.yaml") . prefixOf 10)
  -- convert and save the new transactions so we can add reasons
  forM newTransactionFiles $ \f -> do 
    loadNewTransactionFile "" f
    backupFile f
    putStrLn $ "Expenses file ready! " ++ (f -<.> "yaml")
  -- force insert merged transactions
  forceMergeFiles <- getFromDir "transactions" ".yaml" >>= return . filter ((=="_merge.yaml") . prefixOf 11)
  forM forceMergeFiles $ \f -> do
    e <- loadYamlFile f
    guardNoName f e $ do
      update db . InsertExpenses $ e
      putStrLn $ "Force merged expenses in: " ++ f
      backupFile f
  -- upsert new expenses files
  forM newExpensesFiles $ \f -> do 
    e <- loadYamlFile f :: IO Expenses
    guardNoName f e $ do
      dups <- update db . UpsertExpenses $ e
      case dups of
        [] -> do 
          putStrLn $ f ++ " Successfully loaded!"
        _ -> do
          putStrLn $ "You have duplicate expenses in: " ++ f
          putStrLn $ "I've merged the unique entries for you...you'll find the duplicates in: " ++ dupsFP
          putStrLn $ "Edit this file and rename _dups.yaml to _merge.yaml to force merge the expenses: " ++ dupsFP
          encodeFile dupsFP (e & items .~ dups)
          where
            dupsFP = ((f -<.> "") ++ "_dups.yaml")    
    backupFile f
