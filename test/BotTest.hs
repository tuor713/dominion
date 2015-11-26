-- Run like
-- > cabal build && dist/build/dominion-tests/dominion-tests

import Dominion.Model
import Dominion.Bots
import Dominion.Cards
import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit

starterTableau = map lookupCard ["cellar", "moat", "village", "workshop", "woodcutter", "smithy", "remodel", "militia", "market", "mine"]

botTests = testGroup "gainsToEndGame"
  [ testCase "number of provinces" $
    3 @=?
      gainsToEndGame GameState { piles = Map.fromList $
                                      ([(estate,10),(duchy,10),(province,3),(copper,10),(silver,10),(gold,10)]
                                       ++ zip starterTableau (repeat 10)) }
  , testCase "number of colonies" $
    2 @=?
      gainsToEndGame GameState { piles = Map.fromList $
                                      ([(estate,10),(duchy,10),(province,3),(colony,2),(copper,10),(silver,10),(gold,10)]
                                       ++ zip starterTableau (repeat 10)) }

  , testCase "three pile" $
    3 @=?
      gainsToEndGame GameState { piles = Map.fromList $
                                      ([(estate,10),(duchy,1),(province,5),(colony,4),(copper,10),(silver,10),(gold,2),(curse,0)]
                                       ++ zip starterTableau (repeat 10)) }
  ]

tests = testGroup "Tests" [botTests]

main :: IO ()
main = defaultMain tests