module CardTests where

import Dominion.Model
import Dominion.Cards
import qualified Data.Map.Strict as Map
import qualified Data.List as L

import TestUtil

import Test.Tasty
import Test.Tasty.HUnit

-- Card tests

cardTests = testGroup "Cards"
  [ -- Base
    villageTests,

    -- Intrigue
    testCase "[Duke]'s victory points are equal to the number of [Duchies]" $
      4 @=? cardPoints (lookupCard "duke") Player { deck = map fakeCard [duchy,estate,copper],
                                                    hand = map fakeCard [duchy,duchy],
                                                    discardPile = map fakeCard [gold,duchy],
                                                    inPlay = [],
                                                    inPlayDuration = []},

    -- Adventures
    portTests
    ]


villageTests = testGroup "[Village]"
  [testCase "Draws one card - hand" $
    [e1] @=? hand (playerByName res "Alice"),
   testCase "Draws one card - remaining deck" $
    [e2] @=? deck (playerByName res "Alice"),
   testCase "Increases actions" $
    3 @=? actions (turn res), -- 3 because this is a 'play' outside the context of the action phase
   testCase "Village card ends up in play" $
    [v] @=? inPlay (playerByName res "Alice")]
  where
    v = Card 1 (lookupCard "village")
    [e1,e2] = map (\i -> Card i estate) [2,3]
    state = updatePlayer nullState "Alice" (\p -> p { hand = [v], deck = [e1,e2] })
    res = evalState $ play v "Alice" state

portTests = testGroup "[Port]"
  [testCase "Gets two ports on buy" $
    [port, port] @=? map typ (discardPile (playerByName sut "Alice")),
   testCase "Gets one part on gain" $
    [port] @=? map typ (discardPile (playerByName sut2 "Alice"))
  ]
  where
    port = lookupCard "port"
    state = stubSupply "port" 10 nullState
    sut = evalState $ buy port "Alice" state
    sut2 = evalState $ gain port "Alice" state