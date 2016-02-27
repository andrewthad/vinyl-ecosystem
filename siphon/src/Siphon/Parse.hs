module Siphon.Parse where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BC8
import           Data.Either               (partitionEithers)
import           Data.Functor.Constant
import           Data.Functor.Identity
import           Data.Functor.Product
import qualified Data.List                 as List
import           Data.Tagged.Functor
import           Data.Tuple.TypeLevel
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector
import           Data.Vinyl.Core
import           Data.Vinyl.Plus.TypeLevel (ListAll)
import           GHC.TypeLits
import           Pipes
import           Siphon.Internal           (itraverse)
import           Siphon.Types

-- Field a -> [[ByteString]] -> Either CsvParseError [a]

produceRows :: Monad m => [[ByteString]] -> Producer Input m ()
produceRows [] = return ()
produceRows (bss : bsss) = case bss of
  [] -> yield Newline >> produceRows bsss
  (bs : bssNext) -> yield (Content bs) >> produceRows (bssNext : bsss)

-- I'm almost certain that this can be written way more efficiently
-- if you pattern match on Proxy.
connect :: Monad m => (j -> z) -> (k -> z) -> Producer a m j -> Pipe a b m k -> Producer b m z
connect f g producer pipe = fmap f producer >-> fmap g pipe

rows :: Monad m => Rec Field rs -> Pipe Input (Rec Identity rs) m String
rows rs = (row rs >~ terminateOnLeft)

headedRows :: Monad m
  => Rec (TaggedFunctor Field) rs -> Pipe Input (Rec (TaggedFunctor Identity) rs) m String
headedRows rs = do
  firstRow <- rowList
  case headerIndices rs firstRow of
    Left err -> return err
    Right ixs -> error "uhetn"

headedRowsHelper ::
     Rec (Product (Constant Int) (TaggedFunctor Field)) rs
  -> Vector ByteString
  -> Either String (Rec (TaggedFunctor Identity) rs)
headedRowsHelper (Pair (Constant i) (TaggedFunctor (Field parser)) :& rs) v =
  case v Vector.!? i of
    Just bs -> parser bs
    Nothing -> Left "Missing cell"

headerIndices :: ListAll rs (ConstrainFst KnownSymbol) => Rec proxy rs -> [ByteString] -> Either ByteString (Rec (Constant Int) rs)
headerIndices RNil _ = Right []
headerIndices (r :& rs) bss = case List.elemIndex headerBs bss of
  Just i -> fmap (Constant i :& ) (headerIndices rs bss)
  Nothing -> Left headerBs
  where headerBs = BC8.pack (symbolVal (proxyFst r))

terminateOnLeft :: Monad m => Pipe (Either a b) b m a
terminateOnLeft = do
  e <- await
  case e of
    Left a -> return a
    Right b -> yield b >> terminateOnLeft


rowList :: Monad m => Consumer' Input m [ByteString]
rowList = go []
  where
  go :: forall m. Monad m => [ByteString] -> Consumer' Input m [ByteString]
  go xs = do
    input <- await
    case input of
      Newline -> return xs
      Content x -> go (x : xs)

row :: Monad m => Rec Field rs -> Consumer' Input m (Either String (Rec Identity rs))
row = go
  where
  go :: forall m rs. Monad m => Rec Field rs -> Consumer' Input m (Either String (Rec Identity rs))
  go rs = do
    input <- await
    case input of
      Content bs -> case rs of
        Field parser :& rsNext -> do
          case parser bs of
            Left err -> return (Left err)
            Right res -> do
              e <- go rsNext
              case e of
                Left err -> return (Left err)
                Right resNext -> return (Right (Identity res :& resNext))
        RNil -> return (Left "encountered extra fields")
      Newline -> case rs of
        _ :& _ -> return (Left "encountered end of line")
        RNil   -> return (Right RNil)

-- parseRowsNoFailure :: Field a -> [[ByteString]] -> ([CsvParseError],[a])
-- parseRowsNoFailure field =
--   partitionEithers . map (\(rowNum,bss) -> parseRow rowNum field bss) . zip [0,1..]
--
-- parseRow :: Int -> Field a -> [ByteString] -> Either CsvParseError a
-- parseRow rowNum (Field f) bss = case e of
--   Left (msg,bs) -> Left $ CsvParseError
--     { cpeRow     = rowNum
--     , cpeCol     = colNum
--     , cpeMessage = msg
--     , cpeCell    = bs
--     }
--   Right val -> Right val
--   where ((colNum,rowRemainingBss), e) = f 0 bss

