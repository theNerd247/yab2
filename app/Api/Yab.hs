module Api.Yab where

import Rest.Api
import Api.ApiTypes
import qualified Api.BudgetList as BudgetList
import qualified Api.ExpenseList as ExpenseList
import qualified Api.Expenses as Expenses
import qualified Api.BudgetList.Status as Status
import qualified Api.ExpenseList.ExpenseItem as ExpenseItem
import qualified Api.BudgetList.BudgetItem as BudgetItem

api :: Api YabApi
api = Versioned [(mkVersion 1 0 0, Some1 yab100)]

yab100 :: Router YabApi YabApi
yab100 = root 
  -/ route BudgetList.resource --/ route BudgetItem.resource
    --/ route Status.resource
  -/ route ExpenseList.resource
    --/ route ExpenseItem.resource
  -/ route Expenses.resource
