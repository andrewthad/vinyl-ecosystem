module Siphon.Internal where

mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (a,b) = (f a, b)

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (a,b) = (a, f b)

mapThd3 :: (c -> d) -> (a,b,c) -> (a,b,d)
mapThd3 f (a,b,c) = (a,b,f c)

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f (Right b) = Right (f b)
mapRight _ (Left a) = Left a

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither err Nothing = Left err
maybeToEither _ (Just a) = Right a

itraverse :: Applicative m => (Int -> a -> m b) -> [a] -> m [b]
itraverse f as = traverse (\(i,a) -> f i a) (zip [0,1..] as)

