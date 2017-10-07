module Data.DayDefault where

import Data.Time
import Data.Default

instance Default Day where
  def = fromGregorian 0 0 0

instance Default UTCTime where
  def = UTCTime def 0
