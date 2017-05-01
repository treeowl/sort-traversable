module Main where
import Data.Traversable.Sort.PairingHeap (sortTraversable)
import qualified Data.Traversable.Sort.Vector as V
import Criterion.Main
import Data.List (sort)
import qualified Data.Sequence as Seq
import System.Random
import Data.Foldable

main = do
  g <- getStdGen
  let million = take 1000000 $ randoms g :: [Int]
      million' = Seq.fromList million
      hundredthousand = take 100000 million
      hundredthousand' = Seq.fromList hundredthousand
      tenthousand = take 10000 million
      tenthousand' = Seq.fromList tenthousand
      thousand = take 1000 million
      thousand' = Seq.fromList thousand
  print $ sum $ map length
            [million, hundredthousand,tenthousand,thousand]
  print $ sum $ map (length . toList)
            [million', thousand',hundredthousand',tenthousand',thousand']
  print $ length $ toList hundredthousand
  defaultMain [
    bgroup "1000"
      [ bgroup "list"
        [ bench "Data.List"     $ nf sort thousand
        , bench "HSTrav"        $ nf sortTraversable thousand
        , bench "HSTrav vector" $ nf V.sortTraversable thousand
        ]
      , bgroup "sequence"
        [ bench "sort"           $ nf Seq.sort thousand'
        , bench "unstableSort"   $ nf Seq.unstableSort thousand'
        , bench "HSTrav"         $ nf sortTraversable thousand'
        , bench "HSTrav vector"  $ nf V.sortTraversable thousand'
        ]
      ]
    , bgroup "10000"
        [ bgroup "list"
          [ bench "Data.List"  $ nf sort tenthousand
          , bench "HSTrav"     $ nf sortTraversable tenthousand
          , bench "HSTrav vector"  $ nf V.sortTraversable tenthousand
          ]
        , bgroup "sequence"
          [ bench "sort"           $ nf Seq.sort tenthousand'
          , bench "unstableSort"   $ nf Seq.unstableSort tenthousand'
          , bench "HSTrav"         $ nf sortTraversable tenthousand'
          , bench "HSTrav vector"  $ nf V.sortTraversable tenthousand'
          ]
        ]
    , bgroup "100000"
        [ bgroup "list"
          [ bench "Data.List"  $ nf sort hundredthousand
          , bench "HSTrav"     $ nf sortTraversable hundredthousand
          , bench "HSTrav vector"  $ nf V.sortTraversable hundredthousand
          ]
        , bgroup "sequence"
          [ bench "sort"           $ nf Seq.sort hundredthousand'
          , bench "unstableSort"   $ nf Seq.unstableSort hundredthousand'
          , bench "HSTrav"         $ nf sortTraversable hundredthousand'
          , bench "HSTrav vector"  $ nf V.sortTraversable hundredthousand'
          ]
        ]
    , bgroup "1000000"
        [ bgroup "list"
          [ bench "Data.List"     $ nf sort million
          , bench "HSTrav"        $ nf sortTraversable million
          , bench "HSTrav vector" $ nf V.sortTraversable million
          ]
        , bgroup "sequence"
          [ bench "sort"          $ nf Seq.sort million'
          , bench "unstableSort"  $ nf Seq.unstableSort million'
          , bench "HSTrav"        $ nf sortTraversable million'
          , bench "HSTrav vector" $ nf V.sortTraversable million'
          ]
        ]
      ]

