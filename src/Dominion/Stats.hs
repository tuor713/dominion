{-# LANGUAGE TupleSections #-}
module Dominion.Stats where

import Dominion.Model
import qualified Data.Map.Strict as Map


type StatsCollector a = a -> [GameState] -> a

data Stats = Stats { turnsPerGame :: !(Map.Map Int Int),
                     avgPointsPerTurn :: !(Map.Map PlayerId (Map.Map Int (Int,Int))),
                     winRatio :: !(Map.Map PlayerId Int),
                     totalGames :: !Int,
                     avgMoneyPerTurn :: !(Map.Map PlayerId (Map.Map Int (Int,Int)))
                     }

collectStats :: StatsCollector Stats
collectStats stats game =
  stats { totalGames = totalGames stats + 1,
          turnsPerGame = Map.insertWith (+) (turnNo (last (init game))) 1 (turnsPerGame stats),
          winRatio = Map.insertWith (+) victor 1 (winRatio stats),
          avgPointsPerTurn = newAveragePoints,
          avgMoneyPerTurn = newAverageMoney
          }
  where
    victor = winnerName $ winner (last game)
    winnerName (Win p) = p
    winnerName (Tie _) = "Tie"
    newAverageMoney =
      foldr (\state ->
              Map.adjust
                (\m2 -> Map.insertWith (\(s1,n1) (s2,n2) -> (s1+s2,n1+n2))
                                       (turnNo state)
                                       ((moneySum $ hand (activePlayer state)),1)
                                       m2)
                (activePlayerId state))
            (avgMoneyPerTurn stats)
            (init game)
    newAveragePoints =
      foldr (\state ->
              Map.adjust
                (\m2 -> Map.insertWith (\(s1,n1) (s2,n2) -> (s1+s2,n1+n2)) (turnNo state) ((points (activePlayer state)),1) m2)
                (activePlayerId state))
            (avgPointsPerTurn stats)
            (init game)

emptyStats :: [PlayerId] -> Stats
emptyStats players = Stats { totalGames = 0,
                             avgPointsPerTurn = Map.fromList (map (,Map.empty) players),
                             avgMoneyPerTurn = Map.fromList (map (,Map.empty) players),
                             winRatio = Map.fromList (map (,0) players),
                             turnsPerGame = Map.empty
                             }

statWinRatio :: Stats -> [(PlayerId,Double)]
statWinRatio stats =
  Map.toAscList $ Map.map ((*100.0) . (/ fromIntegral (totalGames stats)) . fromIntegral) (winRatio stats)

statNumberOfGames :: Stats -> Int
statNumberOfGames stats = totalGames stats

statTurnsPerGame :: Stats -> [(Int,Double)]
statTurnsPerGame stats =
  Map.toAscList $ Map.map ((*100.0) . (/ fromIntegral (totalGames stats)) . fromIntegral) (turnsPerGame stats)

statAvgVictoryPerTurn :: Stats -> Map.Map PlayerId [(Int,Double)]
statAvgVictoryPerTurn stats =
  Map.map
    (\m -> Map.toAscList $ Map.map (\(s,n) -> fromIntegral s / fromIntegral n) m)
    (avgPointsPerTurn stats)

statAvgMoneyPerTurn :: Stats -> Map.Map PlayerId [(Int,Double)]
statAvgMoneyPerTurn stats =
  Map.map
    (\m -> Map.toAscList $ Map.map (\(s,n) -> fromIntegral s / fromIntegral n) m)
    (avgMoneyPerTurn stats)
