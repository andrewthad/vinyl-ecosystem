module Siphon.Parse 
  ( rows
  , headedRows
  ) where

import           Control.Monad
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BC8
import           Data.Either               (partitionEithers)
import           Control.Monad.Trans.Except (ExceptT(..), throwE, except)
-- import           Data.Functor.Constant
-- import           Data.Functor.Product
import           Siphon.Functor
import           Data.Functor.Identity
import qualified Data.List                 as List
import           Data.Tagged.Functor
import           Data.Tuple.TypeLevel
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector
import           Data.Vinyl.Core
import           Data.Vinyl.Plus.TypeLevel (ListAll)
import           GHC.TypeLits
import           Siphon.Internal           (itraverse)
import           Siphon.Types
import           Pipes

produceRows :: Monad m => [[ByteString]] -> Producer Input m ()
produceRows [] = return ()
produceRows (bss : bsss) = case bss of
  [] -> yield Newline >> produceRows bsss
  (bs : bssNext) -> yield (Content bs) >> produceRows (bssNext : bsss)

rows :: Monad m => Rec Field rs -> Pipe Input (Rec Identity rs) (ExceptT String m) a
rows rs = row rs >~ cat

headedRows :: forall m (rs :: [(Symbol,*)]) a. 
     (Monad m, ListAll rs (ConstrainFst KnownSymbol))
  => Rec (TaggedFunctor Field) rs 
  -> Pipe Input (Rec (TaggedFunctor Identity) rs) (ExceptT String m) a
headedRows rs = do
  firstRow <- rowList
  ixs <- lift $ ExceptT $ return $ headerIndices rs firstRow
  let h = zipX ixs rs
  forever $ do
    bytestrings <- rowList
    val <- lift $ ExceptT $ return $ headedRowsHelper (Vector.fromList bytestrings) h
    yield val

headedRowsHelper ::
     Vector ByteString
  -> Rec (Product (Constant Int) (TaggedFunctor Field)) rs
  -> Either String (Rec (TaggedFunctor Identity) rs)
headedRowsHelper v (Pair (Constant i) (TaggedFunctor (Field parser)) :& rs) =
  case v Vector.!? i of
    Just bs -> (:&) 
      <$> fmap (TaggedFunctor . Identity) (parser bs)
      <*> headedRowsHelper v rs
    Nothing -> Left "Missing cell"
headedRowsHelper _ RNil = Right RNil

headerIndices :: ListAll rs (ConstrainFst KnownSymbol) 
  => Rec proxy rs -> [ByteString] -> Either String (Rec (Constant Int) rs)
headerIndices RNil _ = Right RNil
headerIndices (r :& rs) bss = case List.elemIndex headerBs bss of
  Just i -> fmap (Constant i :& ) (headerIndices rs bss)
  Nothing -> Left $ BC8.unpack headerBs
  where headerBs = BC8.pack (symbolVal (proxyFst r))

rowList :: Monad m => Consumer' Input m [ByteString]
rowList = go []
  where
  go :: forall m. Monad m => [ByteString] -> Consumer' Input m [ByteString]
  go xs = do
    input <- await
    case input of
      Newline -> return xs
      Content x -> go (x : xs)

row :: Monad m => Rec Field rs -> Consumer' Input (ExceptT String m) (Rec Identity rs)
row = go
  where
  go :: forall m rs. Monad m => Rec Field rs -> Consumer' Input (ExceptT String m) (Rec Identity rs)
  go rs = do
    input <- await
    case input of
      Content bs -> case rs of
        Field parser :& rsNext -> (:&)
          <$> (fmap Identity . lift . ExceptT . return . parser) bs
          <*> go rsNext
        RNil -> lift (throwE "encountered extra fields")
      Newline -> case rs of
        _ :& _ -> lift (throwE "encountered end of line")
        RNil   -> return RNil

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

-- I'm almost certain that this can be written way more efficiently
-- if you pattern match on Proxy.
-- connect :: Monad m => (j -> z) -> (k -> z) -> Producer a m j -> Pipe a b m k -> Producer b m z
-- connect f g producer pipe = fmap f producer >-> fmap g pipe

-- terminateOnLeft :: Monad m => Pipe (Either a b) b m a
-- terminateOnLeft = do
--   e <- await
--   case e of
--     Left a -> return a
--     Right b -> yield b >> terminateOnLeft

