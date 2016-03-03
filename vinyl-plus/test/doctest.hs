import           Test.DocTest (doctest)

main :: IO ()
main = doctest
  [ "-XDataKinds"
  , "-XMagicHash"
  , "-XRankNTypes"
  , "-XPolyKinds"
  , "-XKindSignatures"
  , "-XScopedTypeVariables"
  , "-XTypeOperators"
  , "-XTypeFamilies"
  , "src/Data/Vinyl/Prelude/CoRec.hs"
  , "src/Data/Vinyl/Tagged.hs"
  , "src/Data/Vinyl/Tagged/Proxy/Identity.hs"
  ]
