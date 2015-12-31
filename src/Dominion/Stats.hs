{-# LANGUAGE TupleSections #-}
module Dominion.Stats where

import Dominion.Model
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map


type StatsCollector a = a -> GameState -> a

data Stats = Stats { turnsPerGame :: !(Map.Map Int Int),
                     avgPointsPerTurn :: !(Map.Map PlayerId (Map.Map Int (Int,Int))),
                     winRatio :: !(Map.Map PlayerId Int),
                     totalGames :: !Int,
                     avgMoneyPerTurn :: !(Map.Map PlayerId (Map.Map Int (Int,Int))),
                     maxPoints :: !Int,
                     minPoints :: !Int
                     }

pointProjection :: Player -> Int
pointProjection p = points $ p { deck = deck p ++ Map.findWithDefault [] IslandMat (mats p) }

finalStates :: GameState -> [GameState]
finalStates state =
  map
    (\idx -> state { ply = (ply state) + idx, turnOrder = drop idx (turnOrder state) ++ take idx (turnOrder state) })
    [0..(length (turnOrder state) - 1)]

extractStartOfTurn :: Log -> Maybe GameState
extractStartOfTurn (LogTurn _ _ state) = Just state
extractStartOfTurn _ = Nothing

collectStats :: StatsCollector Stats
collectStats stats finalState =
  stats { totalGames = totalGames stats + 1,
          turnsPerGame = Map.insertWith (+) (turnNo (last game)) 1 (turnsPerGame stats),
          winRatio = Map.insertWith (+) victor 1 (winRatio stats),
          avgPointsPerTurn = newAveragePoints,
          avgMoneyPerTurn = newAverageMoney,
          maxPoints = maxPoints stats + L.maximum (map points (Map.elems (players finalState))),
          minPoints = minPoints stats + L.minimum (map points (Map.elems (players finalState)))
          }
  where
    game = Maybe.catMaybes $ map extractStartOfTurn $ gameLogs finalState
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
            game
    newAveragePoints =
      foldr (\state ->
              Map.adjust
                (\m2 -> Map.insertWith (\(s1,n1) (s2,n2) -> (s1+s2,n1+n2)) (turnNo state) ((pointProjection (activePlayer state)),1) m2)
                (activePlayerId state))
            (avgPointsPerTurn stats)
            (game ++ finalStates finalState)

emptyStats :: [PlayerId] -> Stats
emptyStats players = Stats { totalGames = 0,
                             avgPointsPerTurn = Map.fromList (map (,Map.empty) players),
                             avgMoneyPerTurn = Map.fromList (map (,Map.empty) players),
                             winRatio = Map.fromList (map (,0) players),
                             turnsPerGame = Map.empty,
                             maxPoints = 0,
                             minPoints = 0
                             }

statWinRatio :: Stats -> [(PlayerId,Double)]
statWinRatio stats =
  Map.toAscList $ Map.map ((*100.0) . (/ fromIntegral (totalGames stats)) . fromIntegral) (winRatio stats)

statNumberOfGames :: Stats -> Int
statNumberOfGames stats = totalGames stats

statTurnsPerGame :: Stats -> [(Int,Double)]
statTurnsPerGame stats =
  Map.toAscList $ Map.map ((*100.0) . (/ fromIntegral (totalGames stats)) . fromIntegral) (turnsPerGame stats)

statAvgTurnsPerGame :: Stats -> Double
statAvgTurnsPerGame stats = fromIntegral (Map.foldrWithKey (\k num sum -> sum + k*num) 0 (turnsPerGame stats)) / fromIntegral (totalGames stats)

statMedianTurnsPerGame :: Stats -> Double
statMedianTurnsPerGame stats
  | odd len = fromIntegral $ turns !! (len `quot` 2 + 1)
  | otherwise = fromIntegral (turns !! (len `quot` 2) + turns !! (len `quot` 2 + 1)) / 2.0
  where
    turns = L.sort $ L.concat $ map (\(t,n) -> replicate n t) $ Map.toList $ turnsPerGame stats
    len = length turns

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

statAvgMaxPoints :: Stats -> Double
statAvgMaxPoints stats = fromIntegral (maxPoints stats) / fromIntegral (totalGames stats)

statAvgMinPoints :: Stats -> Double
statAvgMinPoints stats = fromIntegral (minPoints stats) / fromIntegral (totalGames stats)
