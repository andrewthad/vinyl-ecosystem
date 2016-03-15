module Data.Vinyl.Prelude.CoRec where

import           Prelude                      hiding (foldl, head, map, tail,
                                               traverse, unzip, zip, zip3, zip4)

import           Data.Functor.Compose
import           Data.Functor.Constant
import           Data.Functor.Contravariant   (Op (..))
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Monoid                  (Endo (..))
import           Data.Proxy                   (Proxy (Proxy))
import           Data.Typeable                (Typeable)
import           Data.TypeMap                 (TypeMap)
import qualified Data.TypeMap                 as TypeMap
import           Data.Vinyl.Core
import           Data.Vinyl.Functor           (Lift (..))
import           Data.Vinyl.Optic.Plain.Class
import           Data.Vinyl.Plus.TypeLevel    (ListAll)
import           Data.Vinyl.TypeLevel
import           Data.Vinyl.Types

head :: CoRec f (r ': rs) -> Maybe (f r)
head (CoRecThere _) = Nothing
head (CoRecHere v) = Just v

tail :: CoRec f (r ': rs) -> Maybe (CoRec f rs)
tail (CoRecThere rs) = Just rs
tail (CoRecHere _) = Nothing

cons :: CoRec f rs -> CoRec f (r ': rs)
cons = CoRecThere

uncons :: CoRec f (r ': rs) -> Either (f r) (CoRec f rs)
uncons (CoRecHere v) = Left v
uncons (CoRecThere c) = Right c

apply :: Rec (Lift (->) f g) rs -> CoRec f rs -> CoRec g rs
apply (Lift f :& rs) cr = case cr of
  CoRecHere v -> CoRecHere (f v)
  CoRecThere cr' -> CoRecThere (apply rs cr')

map :: (forall x. f x -> g x) -> CoRec f rs -> CoRec g rs
map f (CoRecHere v)  = CoRecHere (f v)
map f (CoRecThere c) = CoRecThere (map f c)

replace :: Rec f rs -> CoRec f rs -> CoRec f rs
replace (r :& rs) (CoRecHere _)   = CoRecHere r
replace (_ :& rs) (CoRecThere cr) = CoRecThere (replace rs cr)

modify :: Rec (Compose Endo f) rs -> CoRec f rs -> CoRec f rs
modify (Compose (Endo g) :& _) (CoRecHere r) = CoRecHere (g r)
modify (_ :& rs) (CoRecThere cr) = CoRecThere (modify rs cr)

modify' :: Rec Endo rs -> CoRec Identity rs -> CoRec Identity rs
modify' (Endo g :& _) (CoRecHere (Identity r)) = CoRecHere (Identity (g r))
modify' (_ :& rs) (CoRecThere cr) = CoRecThere (modify' rs cr)

-- | There is not a actual traverse function for 'CoRec'. Notice how
--   this does not have an 'Applicative' constraint and consequently
--   does not combine contexts. It is provided for symmetry with the
--   traverse function available for 'Rec'.
traverse :: Functor h => (forall x. f x -> h (g x)) -> CoRec f rs -> h (CoRec g rs)
traverse f (CoRecHere v) = CoRecHere <$> f v
traverse f (CoRecThere v) = CoRecThere <$> traverse f v

coalesce :: CoRec (Constant a) rs -> a
coalesce (CoRecHere (Constant a)) = a
coalesce (CoRecThere cr) = coalesce cr

coalesceWith :: (forall a. f a -> b) -> CoRec f rs -> b
coalesceWith f cr = case cr of
  CoRecHere v -> f v
  CoRecThere cr' -> coalesceWith f cr'

coalesceBy :: Rec (Compose (Op b) f) rs -> CoRec f rs -> b
coalesceBy (Compose (Op f) :& _) (CoRecHere v) = f v
coalesceBy (_ :& rs) (CoRecThere cr) = coalesceBy rs cr

-- | Specialization of 'coalesceBy' that is more convenient for
--   working with an 'Identity'-parameterized 'CoRec'. This function
--   can be used to pattern-match on a 'CoRec':
--
-- >>> import Data.Char (ord)
-- >>> :{
-- let handleVal = coalesceBy'
--        $ Op ord
--       :& Op (id :: Int -> Int)
--       :& Op (\b -> if b then 1 else 0)
--       :& RNil
-- :}
--
-- Now we can reduce any 'CoRec' 'Identity' @'[Char,Int,Bool]@
-- to an @Int@.
--
-- >>> handleVal (lift' True)
-- 1
-- >>> handleVal (lift' (44 :: Int))
-- 44
-- >>> handleVal (lift' 'g')
-- 103
--
coalesceBy' :: Rec (Op b) rs -> CoRec Identity rs -> b
coalesceBy' (Op f :& _) (CoRecHere (Identity v)) = f v
coalesceBy' (_ :& rs) (CoRecThere cr) = coalesceBy' rs cr

lift :: RElem r rs i => f r -> CoRec f rs
lift = clift

lift' :: RElem r rs i => r -> CoRec Identity rs
lift' = clift . Identity

-- match' :: (forall a. a -> b) -> CoRec Identity rs -> b
-- match' f cr = case cr of
--   CoRecHere (Identity v) -> f v
--   CoRecThere cr' -> match' f cr'

just :: CoRec f rs -> CoRec (Compose Maybe f) rs
just = map (Compose . Just)

right :: CoRec f rs -> CoRec (Compose (Either a) f) rs
right = map (Compose . Right)

