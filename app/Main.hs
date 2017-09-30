module Main where

import CSV
import Control.Monad.Catch
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Data.Budget
import Data.Data
import Data.Foldable
import Data.Default
import Data.Monoid
import Data.Time
import Data.Yaml hiding ((.~))
import GHC.Generics hiding (to)
import System.FilePath.Posix
import qualified Data.Csv as CSV
import qualified Data.Csv as CSV
import qualified Data.List as DL
import qualified Data.Text as DT

data BadMergeException = BadMergeException String deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Exception BadMergeException

loadYamlFile :: (FromJSON a) => FilePath -> IO [a]
loadYamlFile fp = decodeFileEither fp >>= either throwIO return

loadTransactionFile :: FilePath -> IO [Transaction]
loadTransactionFile = loadCSVFile . ("transactions" </>)

transToExpenses :: (Foldable f) => String -> f Transaction -> Expenses
transToExpenses bname ts = def
  & eBudgetName .~ bname
  & expenses .~ exps 
  where
    exps = toExpense <$> (toList ts)
    toExpense t = def
      & date .~ (t^.tDate)
      & reason .~ (t^.tDesc)
      & eAmount .~ (c - d)
      & expenseType .~ case (c > 0) of
        True -> Income
        _ -> Expense ""
      where
        d = t^.tDebit . to num
        c = t^.tCredit . to num
        num = maybe 0 id

loadNewTransactionFile :: String -> FilePath -> IO ()
loadNewTransactionFile bName fp = loadTransactionFile fp 
  >>= encodeFile ((fp) -<.> "yaml")
    . (:[])
    . (\es -> es & expenses %~ DL.sortOn _date) 
    . (transToExpenses bName)

mergeExpenses :: Expenses -> Expenses -> Expenses
mergeExpenses es1 es2 = es1 & expenses .~ mconcat (mergeDups pred mod ((:[]) <$> es1^.expenses) ((:[]) <$> es2^.expenses))
  where
    pred (a:[]) (b:[]) = (a^.date == b^.date) && (a^.eAmount == b^.eAmount)
    mod (a:[]) bs = (a & reason %~ (++" ?")) : bs

mergeDups :: (a -> a -> Bool) -> (a -> a -> a) -> [a] -> [a] -> [a]
mergeDups pred mod as bs = merges (as ++ bs)
  where
    merges [] = []
    merges (a:xs) = 
      let (as,bs) = DL.partition (pred a) xs
      in (merge (a:as)) : (merges bs)
    merge = foldr1 mod

mergeExpensesFiles :: FilePath -> FilePath -> IO ()
mergeExpensesFiles fp1 fp2 = do
  es1 <- loadYamlFile fp1 
  es2 <- loadYamlFile fp2
  let merged = mergeDups pred mergeExpenses es1 es2
  encodeFile (fp1 -<.> "" ++ "_merged.yaml") merged
  where
    pred a x = x^.eBudgetName == a^.eBudgetName 

main :: IO ()
main = do 
  bs <- loadYamlFile "budgets.yaml"
  es <- loadYamlFile "expenses.yaml" :: IO [Expenses] 
  now <- currentDay
  let
    budgetLoan = bs !! 0
    budgetLiving = (bs !! 1) & startAmount %~ (\x -> x-600)
    n = getEmptyDate budgetLoan
    x = getBalanceAtPeriod (n+1) budgetLoan
    m = getEmptyDate budgetLiving
    y = getBalanceAtPeriod (n+1) budgetLiving
  putStrLn $ "Loan can be payed off in: " 
    ++ (show n) 
    ++ " days - " 
    ++ (show . rateToDay (budgetLoan^.startDate) $ n) 
    ++ " - " ++ (show x)
  putStrLn $ "Your living budget will last: " 
    ++ (show m) 
    ++ " days - " 
    ++ (show . rateToDay (budgetLiving^.startDate) $ m) 
    ++ " - after loan: " ++ (show y)
  lo <- currentBudgetBal budgetLoan
  li <- currentBudgetBal budgetLiving
  putStrLn $ "Current balances should be: "
  putStrLn $ "  Living: " ++ (show $ lo)
  putStrLn $ "  Loan: " ++ (show $ li)
