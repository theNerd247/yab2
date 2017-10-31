module Api.Yab where

import Rest.Api
import Api.ApiTypes
import qualified Api.BudgetList as BudgetList
import qualified Api.ExpenseList as ExpenseList
import qualified Api.Expenses as Expenses
import qualified Api.BudgetList.Status as Status
import qualified Api.ExpenseList.ExpenseItem as ExpenseItem

api :: Api YabApi
api = Versioned [(mkVersion 1 0 0, Some1 yab100)]

yab100 :: Router YabApi YabApi
yab100 = root 
  -/ budgetList --/ status
  -/ expenseList --/ expenseItem
  -/ expenses

budgetList = route BudgetList.resource
expenseList = route ExpenseList.resource
status = route Status.resource
expenses = route Expenses.resource
expenseItem = route ExpenseItem.resource
