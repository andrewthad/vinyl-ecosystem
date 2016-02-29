import Test.DocTest (doctest)

main :: IO ()
main = doctest 
  [ "-XDataKinds"
  , "src/Data/Vinyl/Plus/Tagged.hs"
  ]
