module Api.Yab where

import Rest.Api
import Api.ApiTypes
import qualified Api.Budget as Budget
import qualified Api.Expense as Expense
import qualified Api.Budget.Status as Status

api :: Api YabApi
api = Versioned [(mkVersion 1 0 0, Some1 yab100)]

yab100 :: Router YabApi YabApi
yab100 = root 
  -/ budget --/ status
  -/ expense

budget = route Budget.resource
expense = route Expense.resource
status = route Status.resource
