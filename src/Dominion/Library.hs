module Dominion.Library where

suggestedTableaus :: [(String,[String])]
suggestedTableaus =
  [("First Game",               ["cellar", "market", "militia", "mine", "moat", "remodel", "smithy", "village", "woodcutter", "workshop"]),
   ("Big Money",                ["adventurer", "bureaucrat", "chancellor", "chapel", "feast", "laboratory", "market", "mine", "moneylender", "throne room"]),
   ("Interaction",              ["bureaucrat", "chancellor", "council room", "festival", "library", "militia", "moat", "spy", "thief", "village"]),
   ("Size Distortion",          ["cellar","chapel","feast","gardens","laboratory","thief","village","witch","woodcutter","workshop"]),
   ("Village Square",           ["bureaucrat","cellar","festival","library","market","remodel","smithy","throne room","village","woodcutter"]),

   ("Deconstruction",           ["bridge", "mining village", "remodel", "saboteur", "secret chamber", "spy", "swindler", "thief", "throne room", "torturer"]),
   ("Hand Madness",             ["bureaucrat", "chancellor", "council room", "courtyard", "mine", "militia", "minion", "nobles", "steward", "torturer"]),
   ("Underlings",               ["baron", "cellar", "festival", "library", "masquerade", "minion", "nobles", "pawn", "steward", "witch"]),

   ("Reach for Tomorrow",       ["adventurer", "cellar", "council room", "cutpurse", "ghost ship", "lookout", "sea hag", "spy", "treasure map", "village"]),
   ("Repetition",               ["caravan", "chancellor", "explorer", "festival", "militia", "outpost", "pearl diver", "pirate ship", "treasury", "workshop"]),
   ("Give and Take",            ["ambassador", "fishing village", "haven", "island", "library", "market", "moneylender", "salvager", "smugglers", "witch"]),

   ("Forbidden Arts",           ["apprentice", "familiar", "possession", "university", "cellar", "council room", "gardens", "laboratory", "thief", "throne room"]),
   ("Potion Mixers",            ["alchemist", "apothecary", "golem", "herbalist", "transmute", "cellar", "chancellor", "festival", "militia", "smithy"]),
   ("Chemistry Lesson",         ["alchemist", "golem", "philosopher's stone", "university", "bureaucrat", "market", "moat", "remodel", "witch", "woodcutter"]),

   ("Biggest Money",            ["bank", "grand market", "mint", "royal seal", "venture", "adventurer", "laboratory", "mine", "moneylender", "spy"]),
   ("The King's Army",          ["expand", "goons", "king's court", "rabble", "vault", "bureaucrat", "cellar", "chancellor", "gardens", "village"]),
   ("The Good Life",            ["contraband", "counting house", "hoard", "monument", "mountebank", "bureaucrat", "cellar", "chancellor", "gardens", "village"]),

   ("Bounty of the Hunt",       ["harvest", "horn of plenty", "hunting party", "menagerie", "tournament", "cellar", "festival", "militia", "moneylender", "smithy"]),
   ("Bad Omens",                ["fortune teller", "hamlet", "horn of plenty", "jester", "remake", "adventurer", "bureaucrat", "laboratory", "spy", "throne room"]),
   ("The Jester's Workshop",    ["fairgrounds", "farming village", "horse traders", "jester", "young witch", "feast", "laboratory", "market", "remodel", "workshop"]),

   ("Highway Robbery",          ["cellar", "library", "moneylender", "throne room", "workshop", "highway", "inn", "margrave", "noble brigand", "oasis"]),
   ("Adventures Abroad",        ["adventurer", "chancellor", "festival", "laboratory", "remodel", "crossroads",  "farmland", "fool's gold", "oracle", "spice merchant"]),

   ("High and Low",             ["hermit", "hunting grounds", "mystic", "poor house", "wandering minstrel", "cellar", "moneylender", "throne room", "witch", "workshop"]),
   ("Chivalry and Revelry",     ["altar", "knights", "rats", "scavenger", "squire", "festival", "gardens", "laboratory", "library", "remodel"]),

   ("Victory Dance",            ["bridge", "duke", "great hall", "harem", "ironworks", "masquerade", "nobles", "pawn", "scout", "upgrade"]),
   ("Secret Schemes",           ["conspirator", "harem", "ironworks", "pawn", "saboteur", "shanty town", "steward", "swindler", "trading post", "tribute"]),
   ("Best Wishes",              ["coppersmith", "courtyard", "masquerade", "scout", "shanty town", "steward", "torturer", "trading post", "upgrade", "wishing well"]),

   ("High Seas",                ["bazaar", "caravan", "embargo", "explorer", "haven", "island", "lookout", "pirate ship", "smugglers", "wharf"]),
   ("Buried Treasure",          ["ambassador", "cutpurse", "fishing village", "lighthouse", "outpost", "pearl diver", "tactician", "treasure map", "warehouse", "wharf"]),
   ("Shipwreckers",             ["ghost ship", "merchant ship", "native village", "navigator", "pearl diver", "salvager", "sea hag", "smugglers", "treasury", "warehouse"]),

   ("Beginners",                ["bank", "counting house", "expand", "goons", "monument", "rabble", "royal seal", "venture", "watchtower", "worker's village"]),
   ("Friendly Interactive",     ["bishop", "city", "contraband", "forge", "hoard", "peddler", "royal seal", "trade route", "vault", "worker's village"]),
   ("Big Actions",              ["city", "expand", "grand market", "king's court", "loan", "mint", "quarry", "rabble", "talisman", "vault"]),

   ("Paths to Victory",         ["bishop", "counting house", "goons", "monument", "peddler", "baron", "harem", "pawn", "shanty town", "upgrade"]),
   ("All Along the Watchtower", ["hoard", "talisman", "trade route", "vault", "watchtower", "bridge", "great hall", "mining village", "pawn", "torturer"]),
   ("Lucky Seven",              ["bank", "expand", "forge", "king's court", "vault", "bridge", "coppersmith", "swindler", "tribute", "wishing well"]),

   ("Arts and Crafts",          ["stonemason", "advisor", "baker", "journeyman", "merchant guild", "laboratory", "cellar", "workshop", "festival", "moneylender"]),
   ("Clean Living",             ["butcher", "baker", "candlestick maker", "doctor", "soothsayer", "militia", "thief", "moneylender", "gardens", "village"]),
   ("Gilding the Lily",         ["plaza", "masterpiece", "candlestick maker", "taxman", "herald", "library", "remodel", "adventurer", "market", "chancellor"])
   ]