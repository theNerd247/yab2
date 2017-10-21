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
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import CSV (parseDate)
import Data.Monoid ((<>))
import Data.Acid
import Data.Acid.Abstract
import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Audit
import Data.Bank
import Data.Budget
import Data.Default (def)
import Data.IxSet
import Data.Traversable (forM)
import Snap
import Snap.Snaplet.Heist
import System.Directory
import System.FilePath
import YabAcid
import qualified Data.ByteString.Char8 as B

data App = App
  { _db :: YabAcidState
  } 

makeLenses ''App

loadDB = openLocalStateFrom "/tmp/tst" (def :: YabAcid)

appInit :: SnapletInit App App 
appInit = makeSnaplet "myapp" "Yab snaplet" Nothing $ do
  dbRef <- liftIO $ loadDB
  addRoutes 
    [("expenses/:name", withParam "name" $ expensesByName)
    ,("expenses/:sdate/:edate", withParam "sdate" $ \s -> withParam "edate" $ expensesByDate s)
    ]
  onUnload $ closeAcidState dbRef
  wrapSite (allowVueDev >>)
  return $ App dbRef

allowVueDev :: Handler b v ()
allowVueDev = modifyResponse $ setHeader "Access-Control-Allow-Origin" "http://localhost:8080"

asJSON :: (ToJSON a) => a -> Handler b v ()
asJSON x = do 
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ x

withParam :: B.ByteString -> (B.ByteString -> Handler b v ()) -> Handler b v ()
withParam name f = getParam name >>= maybe err f 
  where
    err = getParams >>= withErr ((B.unpack name) <> " parameter does not exist") . show

withErr :: (ToJSON a) => String -> a -> Handler b v ()
withErr msg extra = do 
  modifyResponse $ setResponseCode 404
  asJSON $ object 
    ["error" .= msg
    ,"error-extra" .= extra
    ]

expensesByName :: B.ByteString -> Handler b App ()
expensesByName name' = method GET $ do
  let name = B.unpack name'
  db <- asks $ view db
  getExpensesByName db name >>= asJSON . toList

expensesByDate :: B.ByteString -> B.ByteString -> Handler b App ()
expensesByDate bsdate bedate = method GET $ do
  let sdate' = B.unpack $ bsdate
  let edate' = B.unpack $ bedate
  fromMaybe (withErr ("cannot parse a date: " ++ sdate' ++ " " ++ edate') (""::String)) $ do 
    sdate <- parseDate sdate'
    edate <- parseDate edate'
    return $ do 
      db <- asks $ view db
      getExpensesByDate db sdate edate >>= asJSON . toList

{-income = 1730.77 :: Amount-}
{-loanAmount = 11352.14 :: Amount-}

{-bank = def & checking .~ 3868.88 & savings .~ 1848.55-}

{-mkBItem e a r = (def :: BudgetItem)-}
    {-& budgetType .~ Expense e-}
    {-& rate .~ r -}
    {-& amount .~ a-}

{-budgetLoan :: BudgetList-}
{-budgetLoan = def -}
  {-& name .~ "Loan"-}
  {-& startDate .~ fromGregorian 2017 09 01-}
  {-& startAmount .~ loanAmount-}
  {-& items .~-}
    {-[mkBItem "loan" (income - (income*0.1 + income*0.2)) 15-}
    {-]-}
  {-where-}

{-budgetLiving :: BudgetList-}
{-budgetLiving = def-}
  {-& name .~ "Living"-}
  {-& startDate .~ fromGregorian 2017 09 01-}
  {-& startAmount .~ income-}
  {-& items .~ -}
    {-[ def & amount .~ 500 & rate .~ 2000-}
    {-, def & amount .~ (income*0.1 + income*0.2) & rate .~ 15-}
    {-, mkBItem "tithe" (income*0.1) 15-}
    {-, mkBItem "taxes"  (income*0.2) 15-}
    {-, mkBItem "rent"      350 31-}
    {-, mkBItem "auto"      20 7-}
    {-, mkBItem "food"      20 7-}
    {-, mkBItem "insurance" 76 31-}
    {-, mkBItem "phone"     30 31 -}
    {-, mkBItem "climbing"  55 31-}
    {-, mkBItem "mentoring" 7  31-}
    {-]-}

{-getFromDir d ext = listDirectory d >>= return . fmap (d </>). filter ((==ext) . takeExtension)-}

{-data NoNameException = NoNameException String-}
  {-deriving (Eq,Ord,Show,Read)-}

{-instance Exception NoNameException-}

{-guardNoName :: (HasName n) => FilePath -> n -> IO a -> IO a-}
{-guardNoName f e m-}
  {-| e^.name == "" = throwM . NoNameException $ "You need to name the expenses! Not merging: " ++ f-}
  {-| otherwise = m-}

{-prefixOf n = reverse . take n . reverse-}

{-backupFile :: FilePath -> IO ()-}
{-backupFile f = renameFile f (f <.> ".bak")-}


main = serveSnaplet defaultConfig appInit
  -- open database
  {--- search for new transaction files-}
  {-newTransactionFiles <- getFromDir "transactions" ".csv"-}
  {--- search for new expenses files-}
  {-newExpensesFiles <- getFromDir "transactions" ".yaml" >>= return . filter ((\x -> x/="merge.yaml" && x/="_dups.yaml") . prefixOf 10)-}
  {--- convert and save the new transactions so we can add reasons-}
  {-forM newTransactionFiles $ \f -> do -}
    {-efp <- loadNewTransactionFile "" f-}
    {-backupFile f-}
    {-putStrLn $ "Expenses file ready! " ++ (efp -<.> "yaml")-}
  {--- force insert merged transactions-}
  {-forceMergeFiles <- getFromDir "transactions" ".yaml" >>= return . filter ((=="_merge.yaml") . prefixOf 11)-}
  {-forM forceMergeFiles $ \f -> do-}
    {-e <- loadYamlFile f-}
    {-guardNoName f e $ do-}
      {-update db . InsertExpenses $ e-}
      {-putStrLn $ "Force merged expenses in: " ++ f-}
      {-backupFile f-}
  {--- upsert new expenses files-}
  {-forM newExpensesFiles $ \f -> do -}
    {-e <- loadYamlFile f :: IO Expenses-}
    {-guardNoName f e $ do-}
      {-dups <- update db . UpsertExpenses $ e-}
      {-case dups of-}
        {-[] -> do -}
          {-putStrLn $ f ++ " Successfully loaded!"-}
        {-_ -> do-}
          {-putStrLn $ "You have duplicate expenses in: " ++ f-}
          {-putStrLn $ "I've merged the unique entries for you...you'll find the duplicates in: " ++ dupsFP-}
          {-putStrLn $ "Edit this file and rename _dups.yaml to _merge.yaml to force merge the expenses: " ++ dupsFP-}
          {-encodeFile dupsFP (e & items .~ dups)-}
          {-where-}
            {-dupsFP = ((f -<.> "") ++ "_dups.yaml")    -}
    {-backupFile f-}
