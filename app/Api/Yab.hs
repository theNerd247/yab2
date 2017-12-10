{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Yab where

import Snap
import Data.Budget
import YabAcid
import Data.Acid
import Api.ApiTypes
import Control.Lens
import Control.Monad.IO.Class
import Api.YabList

data YabSnaplet = YabSnaplet
  { _db :: YabAcidState
  , _budgetItemSnaplet :: Snaplet (YabListSnaplet BudgetItem)
  , _expenseItemSnaplet :: Snaplet (YabListSnaplet ExpenseItem)
  }

makeLenses ''YabSnaplet

instance HasYabAcidSnaplet YabSnaplet where
  snapletYabAcid = db

appInit :: SnapletInit YabSnaplet YabSnaplet 
appInit = makeSnaplet "yab" "Yab snaplet" Nothing $ do
  dbRef <- liftIO $ openLocalStateFrom "/tmp/tst" (def :: YabAcid)
  onUnload $ closeAcidState dbRef
  b <- nestSnaplet "budget" budgetItemSnaplet yabListSnapInit
  e <- nestSnaplet "expense" expenseItemSnaplet yabListSnapInit
  return $ YabSnaplet dbRef b e
