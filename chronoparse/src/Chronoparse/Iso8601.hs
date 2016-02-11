module Chronoparse.Iso8601 where

import           Chronoparse.Combinators
import           Chronoparse.Types
import           Data.Vinyl.Core

date :: Rec Chronoparser '[ 'Year,'None,'Month,'None,'Day]
date = dateSeparatedBy hyphen

time :: Rec Chronoparser '[ 'Hour24, 'None, 'Minute, 'None, 'Second ]
time = timeSeparatedBy colon

datetime :: Rec Chronoparser
  ('[ 'Year,'None,'Month,'None,'Day,'None
    , 'Hour24, 'None, 'Minute, 'None, 'Second ])
datetime = rappend date (capitalT :& time)

-- | Allows any non-numeric separator to be used
--   instead of just the hyphen.
dateLenient :: Rec Chronoparser '[ 'Year,'None,'Month,'None,'Day]
dateLenient = dateSeparatedBy nonDigit

-- | Allows any non-numeric separator to be used
--   instead of just the colon.
timeLenient :: Rec Chronoparser '[ 'Hour24, 'None, 'Minute, 'None, 'Second ]
timeLenient = timeSeparatedBy nonDigit

datetimeLenient :: Rec Chronoparser
  ('[ 'Year,'None,'Month,'None,'Day,'None
    , 'Hour24, 'None, 'Minute, 'None, 'Second ])
datetimeLenient = rappend dateLenient (nonDigit :& timeLenient)

dateSeparatedBy :: Chronoparser 'None -> Rec Chronoparser '[ 'Year,'None,'Month,'None,'Day]
dateSeparatedBy separator = go
  where
  go = yearFourDigit
    :& separator
    :& monthPadded
    :& separator
    :& dayPadded
    :& RNil

timeSeparatedBy :: Chronoparser 'None -> Rec Chronoparser '[ 'Hour24, 'None, 'Minute, 'None, 'Second ]
timeSeparatedBy separator = go
  where
  go = hour24Padded
    :& separator
    :& minutePadded
    :& separator
    :& secondPadded
    :& RNil

