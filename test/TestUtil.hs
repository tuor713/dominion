module TestUtil where

import Dominion.Model
import Dominion.Cards
import qualified Data.Map.Strict as Map
import System.Random (StdGen, mkStdGen)


fakeCard :: CardDef -> Card
fakeCard typ = Card (-1) typ

toState :: GameStep -> GameState
toState (State state) = state
toState (Decision _ state _) = state

mkPiles :: [(CardDef,Int)] -> Map.Map CardDef [Card]
mkPiles cs = Map.fromList $ map (\(def,no) -> (def, map fakeCard $ replicate no def)) cs

zGen :: StdGen
zGen = mkStdGen 0

stubSupply :: String -> Int -> GameState -> GameState
stubSupply cardName num state =
  state { piles = Map.insert c (map (\n -> Card (10000 * (cardTypeId c) + n) c) [1..num]) (piles state) }
  where
    c = lookupCard cardName

evalState :: Simulation -> GameState
evalState sim = toState $ evalSim sim (seedSim zGen)