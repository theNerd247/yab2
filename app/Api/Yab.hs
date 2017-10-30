module Api.Yab where

import Rest.Api
import Api.ApiTypes
import Api.Budget as Budget

api :: Api YabApi
api = Versioned [(mkVersion 1 0 0, Some1 yab100)]

yab100 :: Router YabApi YabApi
yab100 = root 
  -/ budget

budget = route Budget.resource
