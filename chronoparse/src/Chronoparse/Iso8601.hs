module Chronoparse.Iso8601 where

import           Chronoparse.Combinators
import           Chronoparse.Types
import           Data.Vinyl.Core

date :: Rec Chronoparser '[ 'Year,'None,'Month,'None,'Day]
date = go
  where
  go = yearFourDigit
    :& hyphen
    :& monthPadded
    :& hyphen
    :& dayPadded
    :& RNil

time :: Rec Chronoparser '[ 'Hour24, 'None, 'Minute, 'None, 'Second ]
time = go
  where
  go = hour24Padded
    :& hyphen
    :& minutePadded
    :& hyphen
    :& secondPadded
    :& RNil

datetime :: Rec Chronoparser ('[ 'Year,'None,'Month,'None,'Day,'None
                               , 'Hour24, 'None, 'Minute, 'None, 'Second ])
datetime = rappend date (capitalT :& time)

