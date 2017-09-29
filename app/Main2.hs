module Main where

import Data.Budget
import Data.Default
import Control.Lens

budgetTest :: Budget
budgetTest = def
  & name .~ "Test"
  & startAmount .~ 10.00
  & items .~ 
    [mkItem 2 3.4
    ,mkItem 7 4.5
    ,mkItem 6 3.2
    ]
  where
    mkItem r a = def & rate .~ r & amount .~ a

main = putStrLn . show $ budgetTest
