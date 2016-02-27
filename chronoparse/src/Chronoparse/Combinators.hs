module Chronoparse.Combinators where

import           Chronoparse.Types
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString       (Parser)
import           Data.Attoparsec.ByteString.Char8 as Parser
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as ByteString
import qualified Data.ByteString.Char8            as BC8
import           Data.Char                        (ord)
import qualified Data.Char                        as Char
import           Data.Coerce                      (coerce)
import           Data.Vinyl

nonDigit :: Chronoparser 'None
nonDigit = noneParser $ void (Parser.satisfy (not . Char.isDigit))

capitalT :: Chronoparser 'None
capitalT = noneParser $ void (Parser.char 'T')

colon :: Chronoparser 'None
colon = noneParser $ void (Parser.char ':')

slash :: Chronoparser 'None
slash = noneParser $ void (Parser.char '/')

hyphen :: Chronoparser 'None
hyphen = noneParser $ void (Parser.char '-')

yearFourDigit :: Chronoparser 'Year
yearFourDigit = yearParser $ Parser.take 4 >>= requireInt "expected four digit year"

yearTwoDigit :: Int -> Chronoparser 'Year
yearTwoDigit start = yearParser $ fmap (placeYear) (Parser.take 2 >>= requireInt "expected two digit year")
  where startYearInCentury = start `mod` 100
        centuryYear = start `div` 100
        placeYear yr = if yr >= startYearInCentury
          then yr + (centuryYear * 100)
          else yr + ((centuryYear + 1) * 100)

monthPadded :: Chronoparser 'Month
monthPadded = monthParser $ twoDigitPadded 1 12 "month"

monthUnpadded :: Chronoparser 'Month
monthUnpadded = monthParser $ twoDigitUnpadded 1 12 "month"

dayPadded :: Chronoparser 'Day
dayPadded = dayParser $ twoDigitPadded 1 31 "day"

dayUnpadded :: Chronoparser 'Day
dayUnpadded = dayParser $ twoDigitUnpadded 1 31 "day"

hour24Padded :: Chronoparser 'Hour24
hour24Padded = hour24Parser $ twoDigitPadded 0 23 "hour"

hour24Unpadded :: Chronoparser 'Hour24
hour24Unpadded = hour24Parser $ twoDigitUnpadded 0 23 "hour"

minutePadded :: Chronoparser 'Minute
minutePadded = minuteParser $ twoDigitPadded 0 59 "minute"

minuteUnpadded :: Chronoparser 'Minute
minuteUnpadded = minuteParser $ twoDigitUnpadded 0 59 "minute"

secondPadded :: Chronoparser 'Second
secondPadded = secondParser $ twoDigitPadded 0 59 "second"

secondUnpadded :: Chronoparser 'Second
secondUnpadded = secondParser $ twoDigitUnpadded 0 59 "second"

twoDigitPadded :: Int -> Int -> String -> Parser Int
twoDigitPadded low high name =
      Parser.take 2
  >>= requireInt ("expected padded " ++ name)
  >>= requireRange low high
{-# INLINE twoDigitPadded #-}

twoDigitUnpadded :: Int -> Int -> String -> Parser Int
twoDigitUnpadded low high name =
  (Parser.take 2 >>= requireInt ("expected padded " ++ name) >>= requireRange low high)
  <|>
  (fmap (subtract 48 . ord) (Parser.satisfy isDigit))
{-# INLINE twoDigitUnpadded #-}

noneParser :: Parser () -> Chronoparser 'None
noneParser = coerce
{-# INLINE noneParser #-}

yearParser :: Parser Int -> Chronoparser 'Year
yearParser = coerce
{-# INLINE yearParser #-}

monthParser :: Parser Int -> Chronoparser 'Month
monthParser = coerce
{-# INLINE monthParser #-}

dayParser :: Parser Int -> Chronoparser 'Day
dayParser = coerce
{-# INLINE dayParser #-}

hour24Parser :: Parser Int -> Chronoparser 'Hour24
hour24Parser = coerce
{-# INLINE hour24Parser #-}

minuteParser :: Parser Int -> Chronoparser 'Minute
minuteParser = coerce
{-# INLINE minuteParser #-}

secondParser :: Parser Int -> Chronoparser 'Second
secondParser = coerce
{-# INLINE secondParser #-}

requireRange :: Int -> Int -> Int -> Parser Int
requireRange low high val = if val >= low && val <= high
  then return val
  else fail ("value must be between " ++ show low ++ " and " ++ show high)

requireInt :: String -> ByteString -> Parser Int
requireInt err bs = case BC8.readInt bs of
  Nothing -> fail err
  Just (i,bsRemaining) -> if ByteString.null bsRemaining
    then return i
    else fail err

