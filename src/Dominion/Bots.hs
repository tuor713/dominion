module Dominion.Bots where

import Dominion.Model
import Dominion.Cards


-- TODO
-- => the bot returning a GameStep directly is not a very safe pattern
--    it presupposes bots are correctly implemented and not malicious
-- => should we include bot state or is this covered by IO monad ?
type Bot = GameState -> Interaction -> IO GameStep


bigMoneyBot :: Maybe GameStep -> Bot
bigMoneyBot _ _ (Info info next) = return next
bigMoneyBot _ state (Decision (Optional inner next)) = bigMoneyBot (Just next) state (Decision inner)

bigMoneyBot (Just alt) _ (Decision (Choice QBuy choices f))
  | province `elem` choices = return $ f province
  | gold `elem` choices = return $ f gold
  | silver `elem` choices = return $ f silver
  | otherwise = return alt

bigMoneyBot _ _ (Decision (Choices QTreasures choices _ f)) = return $ f choices
