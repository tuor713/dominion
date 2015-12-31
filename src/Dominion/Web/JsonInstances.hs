{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections, ScopedTypeVariables #-}
module Dominion.Web.JsonInstances where

import Dominion.Model
import Dominion.Bots

import qualified Control.Applicative as Ap
import qualified Data.Aeson as J
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V

-- JSON --> Haskell

data PlayerDefinition = HumanPlayer PlayerId | BotPlayer PlayerId String
  deriving (Eq, Ord, Read, Show)

playerDefName (HumanPlayer id) = id
playerDefName (BotPlayer id _) = id

data StartGameReq = StartGameReq { gamePlayers :: [PlayerDefinition],
                                   gameCards :: [String],
                                   gameType :: GameType }
  deriving (Eq,Show)

instance J.FromJSON PlayerDefinition where
  parseJSON (J.Object v) = v J..: "type" >>= \(typ::String) ->
    if typ == "human" then HumanPlayer <$> v J..: "name"
                      else BotPlayer <$> v J..: "name" <*> (v J..:? "bot" J..!= defaultBotId)

  parseJSON _ = Ap.empty

instance J.FromJSON GameType where
  parseJSON (J.String s)
    | s == "colony" = return ColonyGame
    | s == "shelters" = return SheltersGame
    | s == "colonyShelters" = return ColonySheltersGame
    | otherwise = return StandardGame
  parseJSON _ = return StandardGame

instance J.FromJSON StartGameReq where
  parseJSON (J.Object v) = StartGameReq <$>
    v J..: "players" <*>
    v J..: "cards" <*>
    v J..: "type"
  parseJSON _ = Ap.empty


-- Haskell --> JSON

jMap :: J.ToJSON a => Map.Map String a -> J.Value
jMap m = J.object $ map (\(k,v) -> T.pack k J..= J.toJSON v) $ Map.toList m

jString :: String -> J.Value
jString = J.String . T.pack

instance J.ToJSON Card where
  toJSON card = J.toJSON (typ card)

instance J.ToJSON CardDef where
  toJSON card = J.String $ T.pack (cardName card)

instance J.ToJSON Trigger where
  toJSON AttackTrigger = J.String "attack"
  toJSON BuyTrigger = J.String "buy"
  toJSON GainTrigger = J.String "gain"
  toJSON TrashTrigger = J.String "trash"
  toJSON DiscardTrigger = J.String "discard"
  toJSON StartOfTurnTrigger = J.String "startOfTurn"
  toJSON StartOfGameTrigger = J.String "startOfGame"

instance J.ToJSON Location where
  toJSON Supply           = J.toJSON [J.String "supply"]
  toJSON NonSupply        = J.toJSON [J.String "specialSupply"]
  toJSON (Hand p)         = J.toJSON [J.String "hand" , jString p]
  toJSON (Discard p)      = J.toJSON [J.String "discardPile", jString p]
  toJSON Trash            = J.toJSON [J.String "trash"]
  toJSON (TopOfDeck p)    = J.toJSON [J.String "topOfDeck", jString p]
  toJSON (BottomOfDeck p) = J.toJSON [J.String "bottomOfDeck", jString p]
  toJSON InPlay           = J.toJSON [J.String "inPlay"]
  toJSON InPlayDuration   = J.toJSON [J.String "inPlayDuration"]
  toJSON (Mat p mat)      = J.toJSON [J.String "mat", jString p, jString (show mat)]
  toJSON Aside            = J.toJSON [J.String "aside"]

instance J.ToJSON Effect where
  toJSON (EffectPlusCards no)          = J.toJSON [J.String "plusCards", J.toJSON no]
  toJSON (EffectPlusActions no)        = J.toJSON [J.String "plusActions", J.toJSON no]
  toJSON (EffectUseTokens token)       = J.toJSON [J.String "useTokens", jString (show token)]
  toJSON (EffectPlusBuys no)           = J.toJSON [J.String "plusBuys", J.toJSON no]
  toJSON (EffectPlusMoney no)          = J.toJSON [J.String "plusMoney", J.toJSON no]
  toJSON (EffectDiscardNo no)          = J.toJSON [J.String "discardNo", J.toJSON no]
  toJSON (EffectTrashNo no)            = J.toJSON [J.String "trashNo", J.toJSON no]
  toJSON (EffectDiscard card from)     = J.toJSON [J.String "discard", J.toJSON card, J.toJSON from]
  toJSON (EffectBuy card)              = J.toJSON [J.String "buy", J.toJSON card]
  toJSON (EffectGain card to)          = J.toJSON [J.String "gain", J.toJSON card, J.toJSON to]
  toJSON (EffectGainFrom card from to) = J.toJSON [J.String "gainFrom", J.toJSON card, J.toJSON from, J.toJSON to]
  toJSON (EffectPass card from to)     = J.toJSON [J.String "pass", J.toJSON card, J.toJSON from, J.toJSON to]
  toJSON (EffectPut card from to)      = J.toJSON [J.String "put", J.toJSON card, J.toJSON from, J.toJSON to]
  toJSON (EffectTrash card from)       = J.toJSON [J.String "trash", J.toJSON card, J.toJSON from]
  toJSON (EffectReveal card)           = J.toJSON [J.String "reveal", J.toJSON card]
  toJSON (EffectPlayAction card)       = J.toJSON [J.String "playAction", J.toJSON card]
  toJSON (EffectPlayCopy card)         = J.toJSON [J.String "playCopy", J.toJSON card]
  toJSON (EffectPlayTreasure card)     = J.toJSON [J.String "playTreasure", J.toJSON card]
  toJSON (SpecialEffect card)          = J.toJSON [J.String "useAbility", J.toJSON card]
  toJSON (MultiEffect effects)         = J.toJSON [J.String "multiEffect", J.toJSON effects]
  toJSON NullEffect                    = J.toJSON [J.String "nullEffect"]

instance J.ToJSON Decision where
  toJSON (ChooseToUse effect _) = J.object ["type"   J..= J.String "use",
                                            "effect" J..= T.pack (show effect)]

  toJSON (ChooseNumber effect (lo,hi) _ ) = J.object ["type" J..= J.String "chooseNumber",
                                                      "effect" J..= T.pack (show effect),
                                                      "min"    J..= J.toJSON lo,
                                                      "max"    J..= J.toJSON hi
                                                      ]

  toJSON (ChooseCard effect choices _) = J.object ["type"   J..= J.String "chooseCard",
                                                   "cards"  J..= J.Array (V.fromList (map J.toJSON choices)),
                                                   "effect" J..= T.pack (show effect)]

  toJSON (ChooseCards effect choices (lo,hi) _) = J.object ["type"   J..= J.String "chooseCards",
                                                            "cards"  J..= J.Array (V.fromList (map J.toJSON choices)),
                                                            "min"    J..= J.toJSON lo,
                                                            "max"    J..= J.toJSON hi,
                                                            "action" J..= J.toJSON (show effect)
                                                            ]

  toJSON (ChooseToReact card trigger _) = J.object ["type" J..= J.String "react",
                                                    "card" J..= J.toJSON card,
                                                    "trigger" J..= J.toJSON trigger]

  toJSON (ChooseEffects no effects _) = J.object ["type" J..= J.String "chooseEffects",
                                                  "num" J..= J.toJSON no,
                                                  "effects" J..= J.Array (V.fromList (map J.toJSON effects))
                                                  ]

  toJSON (Optional inner _) = J.object ["type"     J..= J.String "optional",
                                        "decision" J..= J.toJSON inner]

instance J.ToJSON Player where
  toJSON player = J.object $ zip ["hand","deck","inPlay","discard"] $ map (jMap . cardMap . ($ player)) [hand, deck, inPlay, discardPile]
    where
      cardMap :: [Card] -> Map.Map String Int
      cardMap cards = Map.mapKeys (cardName . typ) $ Map.fromListWith (+) (map (,1) cards)

instance J.ToJSON GameState where
  toJSON state = J.object ["turnOrder" J..= J.Array (V.fromList $ map (J.String . T.pack) $ turnOrder state),
                           "players" J..= jMap (players state),
                           "piles" J..= (jMap $ Map.mapKeys cardName $ piles state),
                           "ply" J..= J.toJSON (ply state)]

showInfo (vis,info) = "@" ++ show vis ++ " " ++ info

instance J.ToJSON (GameState,Decision) where
  toJSON (state,decision) = J.object ["state"    J..= J.toJSON state,
                                            "decision" J..= J.toJSON decision
                                            ]
