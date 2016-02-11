module Chronoparse.Types where

import           Data.Attoparsec.ByteString (Parser)
import           Data.Time                  (LocalTime (..), TimeOfDay (..),
                                             fromGregorian)
import qualified Data.Time                  as Time
import           Data.Vinyl

newtype Chronoparser c = Chronoparser { getChronoparser :: Parser (Chrono c) }
newtype Chrono c       = Chrono { getChrono :: ChronoF c }

data ChronoType
  = Day
  | Hour12
  | Hour24
  | Minute
  | Month
  | PostMeridiem -- Meaning, is it a PM time
  | Second
  | Year
  | None

type family ChronoF (c :: ChronoType) where
  ChronoF 'Year   = Int
  ChronoF 'Month  = Int
  ChronoF 'Day    = Int
  ChronoF 'Hour24 = Int
  ChronoF 'Hour12 = Int
  ChronoF 'Minute = Int
  ChronoF 'Second = Int
  ChronoF 'PostMeridiem = Bool
  ChronoF 'None = ()

-- Use the canonical (ordered) listing for instances.
class ChronoConvertable (cs :: [ChronoType]) where
  type ChronoTimeRep cs
  fromChronoRec :: Rec Chrono cs -> ChronoTimeRep cs

instance ChronoConvertable '[ 'Day, 'Hour24, 'Minute, 'Month, 'Second, 'Year] where
  type ChronoTimeRep '[ 'Day, 'Hour24, 'Minute, 'Month, 'Second, 'Year] = LocalTime
  fromChronoRec (Chrono day :& Chrono hour :& Chrono minute :& Chrono month :& Chrono sec :& Chrono year :& RNil) =
    LocalTime (fromGregorian (fromIntegral year) month day) (TimeOfDay hour minute (fromIntegral sec))

instance ChronoConvertable '[ 'Day, 'Month, 'Year] where
  type ChronoTimeRep '[ 'Day, 'Month, 'Year] = Time.Day
  fromChronoRec (Chrono day :& Chrono month :& Chrono year :& RNil) =
    fromGregorian (fromIntegral year) month day

type family InsertChrono (c :: ChronoType) (cs :: [ChronoType]) :: [ChronoType] where
  InsertChrono a '[] = '[a]
  InsertChrono a (b ': bs) = InsertChronoCmp (CompareChrono a b) a b bs

type family InsertChronoCmp (o :: Ordering) (c :: ChronoType) (b :: ChronoType) (cs :: [ChronoType]) :: [ChronoType] where
  InsertChronoCmp 'EQ c a cs = (c ': a ': cs)
  InsertChronoCmp 'LT c a cs = (c ': a ': cs)
  InsertChronoCmp 'GT c a cs = (a ': InsertChrono c cs)

type family CompareChrono (c :: ChronoType) (a :: ChronoType) :: Ordering where
  CompareChrono 'Day 'Day = 'EQ
  CompareChrono 'Day 'Hour12 = 'LT
  CompareChrono 'Day 'Hour24 = 'LT
  CompareChrono 'Day 'Minute = 'LT
  CompareChrono 'Day 'Month = 'LT
  CompareChrono 'Day 'None = 'LT
  CompareChrono 'Day 'PostMeridiem = 'LT
  CompareChrono 'Day 'Second = 'LT
  CompareChrono 'Day 'Year = 'LT
  CompareChrono 'Hour12 'Day = 'GT
  CompareChrono 'Hour12 'Hour12 = 'EQ
  CompareChrono 'Hour12 'Hour24 = 'LT
  CompareChrono 'Hour12 'Minute = 'LT
  CompareChrono 'Hour12 'Month = 'LT
  CompareChrono 'Hour12 'None = 'LT
  CompareChrono 'Hour12 'PostMeridiem = 'LT
  CompareChrono 'Hour12 'Second = 'LT
  CompareChrono 'Hour12 'Year = 'LT
  CompareChrono 'Hour24 'Day = 'GT
  CompareChrono 'Hour24 'Hour12 = 'GT
  CompareChrono 'Hour24 'Hour24 = 'EQ
  CompareChrono 'Hour24 'Minute = 'LT
  CompareChrono 'Hour24 'Month = 'LT
  CompareChrono 'Hour24 'None = 'LT
  CompareChrono 'Hour24 'PostMeridiem = 'LT
  CompareChrono 'Hour24 'Second = 'LT
  CompareChrono 'Hour24 'Year = 'LT
  CompareChrono 'Minute 'Day = 'GT
  CompareChrono 'Minute 'Hour12 = 'GT
  CompareChrono 'Minute 'Hour24 = 'GT
  CompareChrono 'Minute 'Minute = 'EQ
  CompareChrono 'Minute 'Month = 'LT
  CompareChrono 'Minute 'None = 'LT
  CompareChrono 'Minute 'PostMeridiem = 'LT
  CompareChrono 'Minute 'Second = 'LT
  CompareChrono 'Minute 'Year = 'LT
  CompareChrono 'Month 'Day = 'GT
  CompareChrono 'Month 'Hour12 = 'GT
  CompareChrono 'Month 'Hour24 = 'GT
  CompareChrono 'Month 'Minute = 'GT
  CompareChrono 'Month 'Month = 'EQ
  CompareChrono 'Month 'None = 'LT
  CompareChrono 'Month 'PostMeridiem = 'LT
  CompareChrono 'Month 'Second = 'LT
  CompareChrono 'Month 'Year = 'LT
  CompareChrono 'None 'Day = 'GT
  CompareChrono 'None 'Hour12 = 'GT
  CompareChrono 'None 'Hour24 = 'GT
  CompareChrono 'None 'Minute = 'GT
  CompareChrono 'None 'Month = 'GT
  CompareChrono 'None 'None = 'EQ
  CompareChrono 'None 'PostMeridiem = 'LT
  CompareChrono 'None 'Second = 'LT
  CompareChrono 'None 'Year = 'LT
  CompareChrono 'PostMeridiem 'Day = 'GT
  CompareChrono 'PostMeridiem 'Hour12 = 'GT
  CompareChrono 'PostMeridiem 'Hour24 = 'GT
  CompareChrono 'PostMeridiem 'Minute = 'GT
  CompareChrono 'PostMeridiem 'Month = 'GT
  CompareChrono 'PostMeridiem 'None = 'GT
  CompareChrono 'PostMeridiem 'PostMeridiem = 'EQ
  CompareChrono 'PostMeridiem 'Second = 'LT
  CompareChrono 'PostMeridiem 'Year = 'LT
  CompareChrono 'Second 'Day = 'GT
  CompareChrono 'Second 'Hour12 = 'GT
  CompareChrono 'Second 'Hour24 = 'GT
  CompareChrono 'Second 'Minute = 'GT
  CompareChrono 'Second 'Month = 'GT
  CompareChrono 'Second 'None = 'GT
  CompareChrono 'Second 'PostMeridiem = 'GT
  CompareChrono 'Second 'Second = 'EQ
  CompareChrono 'Second 'Year = 'LT
  CompareChrono 'Year 'Day = 'GT
  CompareChrono 'Year 'Hour12 = 'GT
  CompareChrono 'Year 'Hour24 = 'GT
  CompareChrono 'Year 'Minute = 'GT
  CompareChrono 'Year 'Month = 'GT
  CompareChrono 'Year 'None = 'GT
  CompareChrono 'Year 'PostMeridiem = 'GT
  CompareChrono 'Year 'Second = 'GT
  CompareChrono 'Year 'Year = 'EQ

class InsertChronoClass (c :: ChronoType) (cs :: [ChronoType]) where
  insertChrono :: Chrono c -> Rec Chrono cs -> Rec Chrono (InsertChrono c cs)

instance InsertChronoClass c '[] where
  insertChrono a RNil = a :& RNil

instance InsertChronoClass 'Day ('Day ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Day ('Hour12 ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Day ('Hour24 ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Day ('Minute ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Day ('Month ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Day ('None ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Day ('PostMeridiem ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Day ('Second ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Day ('Year ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Hour12 cs => InsertChronoClass 'Hour12 ('Day ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Hour12 ('Hour12 ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Hour12 ('Hour24 ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Hour12 ('Minute ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Hour12 ('Month ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Hour12 ('None ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Hour12 ('PostMeridiem ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Hour12 ('Second ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Hour12 ('Year ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Hour24 cs => InsertChronoClass 'Hour24 ('Day ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Hour24 cs => InsertChronoClass 'Hour24 ('Hour12 ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Hour24 ('Hour24 ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Hour24 ('Minute ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Hour24 ('Month ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Hour24 ('None ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Hour24 ('PostMeridiem ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Hour24 ('Second ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Hour24 ('Year ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Minute cs => InsertChronoClass 'Minute ('Day ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Minute cs => InsertChronoClass 'Minute ('Hour12 ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Minute cs => InsertChronoClass 'Minute ('Hour24 ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Minute ('Minute ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Minute ('Month ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Minute ('None ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Minute ('PostMeridiem ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Minute ('Second ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Minute ('Year ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Month cs => InsertChronoClass 'Month ('Day ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Month cs => InsertChronoClass 'Month ('Hour12 ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Month cs => InsertChronoClass 'Month ('Hour24 ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Month cs => InsertChronoClass 'Month ('Minute ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Month ('Month ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Month ('None ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Month ('PostMeridiem ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Month ('Second ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Month ('Year ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'None cs => InsertChronoClass 'None ('Day ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'None cs => InsertChronoClass 'None ('Hour12 ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'None cs => InsertChronoClass 'None ('Hour24 ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'None cs => InsertChronoClass 'None ('Minute ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'None cs => InsertChronoClass 'None ('Month ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'None ('None ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'None ('PostMeridiem ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'None ('Second ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'None ('Year ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'PostMeridiem cs => InsertChronoClass 'PostMeridiem ('Day ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'PostMeridiem cs => InsertChronoClass 'PostMeridiem ('Hour12 ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'PostMeridiem cs => InsertChronoClass 'PostMeridiem ('Hour24 ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'PostMeridiem cs => InsertChronoClass 'PostMeridiem ('Minute ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'PostMeridiem cs => InsertChronoClass 'PostMeridiem ('Month ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'PostMeridiem cs => InsertChronoClass 'PostMeridiem ('None ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'PostMeridiem ('PostMeridiem ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'PostMeridiem ('Second ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'PostMeridiem ('Year ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Second cs => InsertChronoClass 'Second ('Day ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Second cs => InsertChronoClass 'Second ('Hour12 ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Second cs => InsertChronoClass 'Second ('Hour24 ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Second cs => InsertChronoClass 'Second ('Minute ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Second cs => InsertChronoClass 'Second ('Month ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Second cs => InsertChronoClass 'Second ('None ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Second cs => InsertChronoClass 'Second ('PostMeridiem ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Second ('Second ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Second ('Year ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs
instance InsertChronoClass 'Year cs => InsertChronoClass 'Year ('Day ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Year cs => InsertChronoClass 'Year ('Hour12 ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Year cs => InsertChronoClass 'Year ('Hour24 ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Year cs => InsertChronoClass 'Year ('Minute ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Year cs => InsertChronoClass 'Year ('Month ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Year cs => InsertChronoClass 'Year ('None ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Year cs => InsertChronoClass 'Year ('PostMeridiem ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Year cs => InsertChronoClass 'Year ('Second ': cs) where
  insertChrono a (b :& cs) = b :& insertChrono a cs
instance InsertChronoClass 'Year ('Year ': cs) where
  insertChrono a (b :& cs) = a :& b :& cs

type family CanonizeChronos (c :: [ChronoType]) :: [ChronoType] where
  CanonizeChronos '[] = '[]
  CanonizeChronos (a ': as) = InsertChrono a (CanonizeChronos as)

class CanonizeChronosClass (cs :: [ChronoType]) where
  canonizeChronos :: Rec Chrono cs -> Rec Chrono (CanonizeChronos cs)

instance CanonizeChronosClass '[] where
  canonizeChronos  RNil = RNil

instance (InsertChronoClass c (CanonizeChronos cs), CanonizeChronosClass cs) => CanonizeChronosClass (c ': cs) where
  canonizeChronos (c :& cs) = insertChrono c (canonizeChronos cs)

type family StripNones (cs :: [ChronoType]) :: [ChronoType] where
  StripNones '[] = '[]
  StripNones ('None ': cs) = StripNones cs
  StripNones ('Day ': cs) = 'Day ': StripNones cs
  StripNones ('Hour12 ': cs) = 'Hour12 ': StripNones cs
  StripNones ('Hour24 ': cs) = 'Hour24 ': StripNones cs
  StripNones ('Minute ': cs) = 'Minute ': StripNones cs
  StripNones ('Month ': cs) = 'Month ': StripNones cs
  StripNones ('PostMeridiem ': cs) = 'PostMeridiem ': StripNones cs
  StripNones ('Second ': cs) = 'Second ': StripNones cs
  StripNones ('Year ': cs) = 'Year ': StripNones cs

class StripNonesClass (cs :: [ChronoType]) where
  stripNones :: Rec Chrono cs -> Rec Chrono (StripNones cs)

instance StripNonesClass '[] where
  stripNones RNil = RNil

instance StripNonesClass cs => StripNonesClass ('None ': cs) where
  stripNones (_ :& cs) = stripNones cs

instance StripNonesClass cs => StripNonesClass ('Day ': cs) where
  stripNones (c :& cs) = c :& stripNones cs
instance StripNonesClass cs => StripNonesClass ('Hour12 ': cs) where
  stripNones (c :& cs) = c :& stripNones cs
instance StripNonesClass cs => StripNonesClass ('Hour24 ': cs) where
  stripNones (c :& cs) = c :& stripNones cs
instance StripNonesClass cs => StripNonesClass ('Minute ': cs) where
  stripNones (c :& cs) = c :& stripNones cs
instance StripNonesClass cs => StripNonesClass ('Month ': cs) where
  stripNones (c :& cs) = c :& stripNones cs
instance StripNonesClass cs => StripNonesClass ('PostMeridiem ': cs) where
  stripNones (c :& cs) = c :& stripNones cs
instance StripNonesClass cs => StripNonesClass ('Second ': cs) where
  stripNones (c :& cs) = c :& stripNones cs
instance StripNonesClass cs => StripNonesClass ('Year ': cs) where
  stripNones (c :& cs) = c :& stripNones cs





