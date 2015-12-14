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
    [sut,sutColony] = map (\typ -> evalSim (mkGame typ ["Alice","Bob"] starterTableau) (mkStdGen 0)) [StandardGame, ColonyGame]
    sutPotion = evalSim (mkGame StandardGame ["Alice","Bob"] (lookupCard "vineyard":tail starterTableau)) (mkStdGen 0)

toState :: GameStep -> GameState
toState (State state) = state
toState _ = error "Trying to extract state from decision"

gameEndTest = testGroup "Game End"
  [ testCase "Tie at the start of the game" $
    Tie ["Alice","Bob"] @=? winner sut,

    testCase "Win after one turn is second player" $
    Win "Bob" @=? winner next,

    testCase "Win with one extra estate" $
    Win "Alice" @=? winner afterEstate
    ]
  where
    sut = evalSim (mkGame StandardGame ["Alice","Bob"] starterTableau) (mkStdGen 0)
    next = evalSim (nextTurn sut) (mkStdGen 0)
    afterEstate = toState $ evalSim ((gain estate &&& cleanupPhase) "Alice" sut) (mkStdGen 0)


-- Cards

fakeCard :: CardDef -> Card
fakeCard typ = Card 0 typ

cardTests = testGroup "Cards"
  [ testCase "[Duke]'s victory points are equal to the number of [Duchies]" $
      4 @=? cardPoints (lookupCard "duke") Player { deck = map fakeCard [duchy,estate,copper],
                                                    hand = map fakeCard [duchy,duchy],
                                                    discardPile = map fakeCard [gold,duchy],
                                                    inPlay = []}]


-- Bots

botTests = testGroup "Bots"
  [ gainsToEndGameTest ]

mkPiles :: [(CardDef,Int)] -> Map.Map CardDef [Card]
mkPiles cs = Map.fromList $ map (\(def,no) -> (def, map fakeCard $ replicate no def)) cs

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
