module Dominion.Bots
  (module Dominion.Bots.Util,
   module Dominion.Bots.Strategies,
   botLibrary)
where

import Dominion.Bots.Strategies
import Dominion.Bots.Util

import Dominion.Model



botLibrary :: [(String, PlayerId -> IO Bot)]
botLibrary =
  [("Big Money", return . aiBot . betterBigMoney),
   ("Big Money Smithy", return . aiBot . bigSmithy),
   ("Double Jack", return . aiBot . doubleJack),
   ("Double Militia", return . aiBot . doubleMilitia),
   ("Chapel Witch", return . aiBot . chapelWitch)]

