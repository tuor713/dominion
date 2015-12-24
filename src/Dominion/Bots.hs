module Dominion.Bots
  (module Dominion.Bots.Util,
   module Dominion.Bots.Strategies)
where

import Dominion.Bots.Strategies
import Dominion.Bots.Util

botLibrary :: [(String, PlayerId -> AIBot)]
botLibrary =
  [("Big Money", betterBigMoney),
   ("Big Money Smithy", bigSmithy),
   ("Double Jack", doubleJack),
   ("Double Militia", doubleMilitia),
   ("Chapel Witch", chapelWitch)]
