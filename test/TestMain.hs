-- Run like
-- > cabal test && dist/build/dominion-tests/dominion-tests

import Dominion.Model
import Dominion.Bots
import Dominion.Cards
import System.Random (mkStdGen)
import qualified Data.Map.Strict as Map
import qualified Data.List as L

import Test.Tasty
import Test.Tasty.HUnit

import CardTests
import TestUtil



main :: IO ()
main = defaultMain tests

tests = testGroup "Tests" [gameTests, cardTests, botTests]


-- Core game engine

gameTests = testGroup "Game"
  [ initialStateTest, gameEndTest ]

starterTableau = map lookupCard ["cellar", "moat", "village", "workshop", "woodcutter", "smithy", "remodel", "militia", "market", "mine"]


initialStateTest = testGroup "Game creation"
  [ testCase "All players start with 5 cards in hand" $
    [5] @=? L.nub (map (length . hand) $ Map.elems $ players sut),

    testCase "All players start with 10 cards in total" $
    [10] @=? L.nub (map (length . allCards) $ Map.elems $ players sut),

    testCase "[Copper] and [Estate] make up the starting deck" $
    [[copper, estate]] @=? L.nub (map (L.nub . map typ . allCards) $ Map.elems $ players sut),

    testCase "A game has 17 piles (10 kingdom cards + 7 basic supply" $
    17 @=? Map.size (piles sut),

    testCase "A game with potion cards has 18 piles, one extra for potion" $
    18 @=? Map.size (piles sutPotion),

    testCase "A colony game has 19 piles" $
    19 @=? Map.size (piles sutColony)
    ]
  where
    [sut,sutColony] = map (\typ -> eval (mkGame typ ["Alice","Bob"] starterTableau)) [StandardGame, ColonyGame]
    sutPotion = eval (mkGame StandardGame ["Alice","Bob"] (lookupCard "vineyard":tail starterTableau))

gameEndTest = testGroup "Game End"
  [ testCase "Tie at the start of the game" $
    Tie ["Alice","Bob"] @=? winner sut,

    testCase "Win after one turn is second player" $
    Win "Bob" @=? winner next,

    testCase "Win with one extra estate" $
    Win "Alice" @=? winner afterEstate
    ]
  where
    sut = eval (mkGame StandardGame ["Alice","Bob"] starterTableau)
    next = evalState (putGameState' sut >> gameState' >>= nextTurn)
    afterEstate = evalState (putGameState' sut >> (gain estate &&& cleanupPhase) "Alice")



-- Bots

botTests = testGroup "Bots"
  [ gainsToEndGameTest ]

gainsToEndGameTest = testGroup "gainsToEndGame"
  [ testCase "number of provinces" $
    3 @=?
      gainsToEndGame GameState { piles = mkPiles ([(estate,10),(duchy,10),(province,3),(copper,10),(silver,10),(gold,10)]
                                                  ++ zip starterTableau (repeat 10)) }
  , testCase "number of colonies" $
    2 @=?
      gainsToEndGame GameState { piles = mkPiles
                                      ([(estate,10),(duchy,10),(province,3),(colony,2),(copper,10),(silver,10),(gold,10)]
                                       ++ zip starterTableau (repeat 10)) }

  , testCase "three pile" $
    3 @=?
      gainsToEndGame GameState { piles = mkPiles
                                      ([(estate,10),(duchy,1),(province,5),(colony,4),(copper,10),(silver,10),(gold,2),(curse,0)]
                                       ++ zip starterTableau (repeat 10)) }
  ]
