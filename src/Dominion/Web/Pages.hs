{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Dominion.Web.Pages where

import Dominion.Model
import Dominion.Cards
import Dominion.Stats
import Dominion.Bots (botLibrary)

import Control.Monad (forM_, when)

import qualified Data.ByteString.Char8 as C8
import Data.Char (toLower)
import qualified Data.Either as Either
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.String (fromString)

import Snap.Http.Server (snapServerVersion)

import Text.Blaze.Html (toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (MarkupM(Parent))

import Text.Printf (printf)


-- Utilities

cardImagePath :: CardDef -> String
cardImagePath card = "/static/images/cards/"
  ++ filter (not . (=='\'')) (map (\c -> if c == ' ' || c == '-' then '_' else toLower c) (cardName card))
  ++ ".jpeg"

dataToJavaScriptArray :: (Show a, Show b) => [(a,b)] -> String
dataToJavaScriptArray xs =
  C8.unpack $
    "[" `C8.append`
    (C8.intercalate "," $ map (\(x,y) -> C8.pack ("{name:"++ show x ++ ", value:" ++ show y ++ "}")) xs)
    `C8.append` "]"

svg :: H.Html -> H.Html
svg = Parent "svg" "<svg" "</svg>"

input :: H.Html -> H.Html
input = Parent "input" "<input" "</input>"

-- Fragments

htmlHeader :: H.Html
htmlHeader = H.head $ do
  H.link H.! A.href "/static/site.css" H.! A.rel "stylesheet"
  H.script H.! A.src "/static/js/jquery-2.1.4.min.js" $ ""

  H.link H.! A.href "/static/libs/semanticui/semantic.min.css" H.! A.rel "stylesheet" H.! A.type_ "text/css"
  H.script H.! A.src "/static/libs/semanticui/semantic.min.js" $ ""

  H.script H.! A.src "/static/js/play.js" $ ""
  H.script H.! A.src "/static/js/d3.v3.min.js" H.! A.charset "utf-8" $ ""
  H.script H.! A.src "/static/js/graph.js" H.! A.charset "utf-8" $ ""

template :: String -> H.Html -> H.Html
template link inner =
  H.html $ do
    htmlHeader
    H.body $ do
      H.div H.! A.class_ "ui fixed inverted menu" $ do
        H.div H.! A.class_ "ui container" $ do
          H.a H.! A.class_ (if "/" == link then "active item" else "item") H.! A.href "/" $ "Home"

          H.div H.! A.class_ "ui simple dropdown item" $ do
            _ <- "Play"
            H.i H.! A.class_ "dropdown icon" $ ""
            H.div H.! A.class_ "menu" $ do
              H.a H.! A.class_ "item" H.! A.href "/game/play" $ "Choose tableau"
              H.a H.! A.class_ "item" H.! A.href "/game/suggested" $ "Pick pre-made tableau"

          H.a H.! A.class_ (if "/simulation" == link then "active item" else "item") H.! A.href "/simulation" $ "Simulation"

      H.div H.! A.class_ "ui grid" $ do
        inner

      H.footer H.! A.class_ "ui inverted vertical footer segment" $ do
        H.div H.! A.class_ "ui inverted segment" $ do
          H.p $ toHtml ("Powered by Snap (" ++ C8.unpack snapServerVersion ++ ")")


-- Pages

numberToText :: Int -> String
numberToText i =
  (Map.fromList [(1,"one"), (2,"two"), (3,"three"), (4,"four"), (5,"five"), (6,"six"), (7,"seven"),
                 (8,"eight"), (9,"nine"), (10, "ten"), (11, "eleven"), (12, "twelve"), (13, "thirteen"), (14, "fourteen"),
                 (15,"fifteen"), (16,"sixteen")])
  Map.! i

column :: Int -> H.Html -> H.Html
column n = H.div H.! A.class_ (fromString (numberToText n ++ " wide column"))

htmlHomePage :: H.Html
htmlHomePage =
  template "/" $ do
    column 1 $ ""
    column 14 $ do
      H.section $ do
        H.h3 "Dominion Play Server"
        H.p "A small experimental Dominion server for both bot and human play."


passButton = H.button H.! A.class_ "ui teal button" H.! A.onclick "pass();" $ "Pass"

decisionHtml :: Bool -> Decision -> H.Html
decisionHtml _ (Optional inner _) = decisionHtml True inner

decisionHtml allowsPass (ChooseNumber effect (lo,hi) _) =
  H.div $ do
    toHtml $ "Choose how many to use for " ++ show effect
    H.div H.! A.class_ "centered" $ do
      forM_ [lo..hi] $ \n ->
        H.button H.! A.class_ "ui primary button" H.! A.onclick (fromString ("choose('" ++ show n ++ "');")) $ fromString (show n)
      when allowsPass $ passButton

decisionHtml allowsPass (ChooseToUse effect _) =
  H.div $ do
    toHtml $ "Use " ++ show effect
    H.div H.! A.class_ "centered" $ do
      H.button H.! A.class_ "ui button" H.! A.onclick "choose('true');" $ "Yes"
      H.button H.! A.class_ "ui button" H.! A.onclick "choose('false');" $ "No"
      when allowsPass $ passButton

decisionHtml allowsPass (ChooseToReact card trigger _) =
  H.div $ do
    toHtml $ "Use " ++ show card ++ "'s reaction to respond to " ++ show trigger
    H.div H.! A.class_ "centered" $ do
      H.button H.! A.class_ "ui button" H.! A.onclick "choose('true');" $ "Yes"
      H.button H.! A.class_ "ui button" H.! A.onclick "choose('false');" $ "No"
      when allowsPass $ passButton

decisionHtml allowsPass (ChooseCard effect choices _) =
  H.div $ do
    toHtml $ "Choose a cards to " ++ show effect ++ ":"

    H.div H.! A.id "choices" H.! A.class_ "centered" $ do
      forM_ choices $ \card -> do
        H.input H.! A.type_ "image"
                H.! A.onclick (fromString ("choose(\""++ cardName (typ card) ++ "\",1,1);"))
                H.! A.style "margin: 5px; width: 100px; height: 159px"
                H.! A.src (fromString (cardImagePath (typ card)))

    H.div H.! A.class_ "centered" $ do
      when allowsPass $ passButton


decisionHtml allowsPass (ChooseCards effect choices (lo,hi) _) =
  H.div $ do
    toHtml $ "Choose between " ++ show lo ++ " and " ++ show hi ++ " cards to " ++ show effect ++ ":"

    H.div H.! A.class_ "centered" $ do
      H.div H.! A.id "choices" $ do
        forM_ choices $ \card -> do
          H.input H.! A.type_ "image"
                  H.! A.name (fromString (cardName (typ card)))
                  H.! A.class_ "cardbox"
                  H.! A.style "margin: 5px; width: 100px; height: 159px"
                  H.! A.src (fromString (cardImagePath (typ card)))

    H.div H.! A.class_ "centered" $ do
      if length choices <= hi
        then H.button H.! A.class_ "ui primary button"
                      H.! A.onclick (fromString ("choose(\""++ L.intercalate "," (map (cardName . typ) choices) ++ "\");"))
                      $ "All"
        else return ()
      H.button H.! A.class_ "ui secondary button"
               H.! A.onclick (fromString ("choices('choices'," ++ show lo ++ "," ++ show hi ++ ");")) $ "Go"
      when allowsPass $ passButton

decisionHtml allowsPass (ChooseEffects no effects _) =
  H.div $ do
    H.div H.! A.id "choices" $ do
      H.label $ toHtml $ "Choose " ++ show no ++ ":"
      H.div H.! A.class_ "grouped fields" $ do
        forM_ (zip [0..] effects) $ \(no::Int,effect) ->
          H.div H.! A.class_ "field" $ do
            H.div H.! A.class_ "ui checkbox" $ do
              H.input H.! A.type_ "checkbox" H.! A.name (fromString (show no))
              H.label $ toHtml (show effect)
    H.div $ do
      H.button H.! A.class_ "ui primary button"
               H.! A.onclick (fromString ("choices('choices'," ++ show no ++ "," ++ show no ++ ");")) $ "Go"
      when allowsPass $ passButton


compareCard :: CardDef -> CardDef -> Ordering
compareCard c1 c2 =
  compare (moneyCost (cost nullState c1)) (moneyCost (cost nullState c2))
  `mappend` compare (potionCost (cost nullState c1)) (potionCost (cost nullState c2))
  `mappend` compare (cardName c1) (cardName c2)

showCards :: Map.Map CardDef Int -> H.Html
showCards cards =
  forM_ (L.sortBy (\(c1,_) (c2,_) -> compareCard c1 c2) (Map.toList cards))
    $ \(card,num) ->
      H.span H.! A.class_ (fromString ("cardlink " ++ L.intercalate " " (map show (types card)))) $ do
        toHtml (show num ++ " " ++ cardName card)
        H.img H.! A.src (fromString (cardImagePath card))

cardListToMap :: [Card] -> Map.Map CardDef Int
cardListToMap cards = Map.fromListWith (+) $ map ((,1) . typ) cards

logsSidebar :: [(String,String)] -> H.Html
logsSidebar infos =
  column 5 $ do
    H.section H.! A.class_ "logs" $ do
      H.h3 "Game Log"
      H.div $ do
        forM_  infos $ \(vis,msg) ->
          H.div H.! A.class_ (fromString (if L.isPrefixOf "Turn" msg then "log turn" else "log")) $ do
            H.span H.! A.class_ "player" $ toHtml ("@" ++ vis ++ " ")
            H.span $ toHtml msg


htmlDecision :: PlayerId -> (GameState,[Info],Decision) -> H.Html
htmlDecision p (state,infos,decision) =
  template "/play" $ do
    column 1 $ ""
    column 10 $ do
      H.div H.! A.id "flash" $ ""
      H.section H.! A.class_ "decision" $ do
        H.h3 "Decision"
        decisionHtml False decision

      H.section H.! A.class_ "state" $ do
        H.p $ do
          H.h4 "Turn: "
          H.span $ toHtml $ "Turn: " ++ show (turnNo state) ++ ", "
          H.span $ toHtml $ "Actions: " ++ show (actions (turn state)) ++ ", "
          H.span $ toHtml $ "Buys: " ++ show (buys (turn state)) ++ ", "
          H.span $ toHtml $ "Money: " ++ show (money (turn state))
          when (0 < potions (turn state)) $ H.span $ toHtml $ ", Potions: " ++ show (potions (turn state))

        H.p $ do
          H.h4 "Tableau:"
          showCards $ Map.map length (piles state)
          when (not (null (trashPile state))) $ do
            H.h4 "Trash"
            showCards $ Map.fromListWith (+) $ map ((,1) . typ) (trashPile state)

      forM_ (zip (Map.toAscList (players state)) [0..]) $ \((pid,player),no::Int) -> do
        H.h4 H.! A.class_ (fromString (if no == 0 then "ui top attached header" else "ui attached header")) $ toHtml $ "Player - " ++ pid
        H.div H.! A.class_ "ui attached segment" $ do
          when (not (Map.null (tokens player))) $ do
            H.h5 "Tokens: "
            H.p $
              forM_ (Map.toList (tokens player)) $ \(token,num) -> do
                toHtml $ show token ++ ": " ++ show num
                H.br
          when (not (null (hand player))) $ do
            H.h5 "Hand: "
            showCards (cardListToMap (hand player))
          when (not (null (inPlay player ++ Either.lefts (inPlayDuration player)))) $ do
            H.h5 "InPlay: "
            showCards (cardListToMap (inPlay player ++ Either.lefts (inPlayDuration player)))
          when (not (null (deck player))) $ do
            H.h5 "Deck: "
            showCards (cardListToMap (deck player))
          when (not (null (discardPile player))) $ do
            H.h5 "Discard: "
            showCards (cardListToMap (discardPile player))

          when (not (null $ Map.findWithDefault [] IslandMat (mats player))) $ do
            H.h5 $ "Island Mat"
            showCards (cardListToMap (mats player Map.! IslandMat))

          when (not (null $ Map.findWithDefault [] NativeVillageMat (mats player)) && pid == p) $ do
            H.h5 $ "Native Village Mat"
            showCards (cardListToMap (mats player Map.! NativeVillageMat))

    logsSidebar $
      if length infos > 20
      then ("", "[...]"):map (\(vis,msg) -> (show vis, msg)) (drop (max 0 (length infos - 20)) infos)
      else map (\(vis,msg) -> (show vis, msg)) infos

niceDouble :: Double -> String
niceDouble d = printf "%.2f" d

ratio :: Int -> Int -> Double
ratio num denom = fromIntegral num / fromIntegral denom

percentage :: Int -> Int -> Double
percentage num total = 100 * ratio num total

showDeckStats :: Player -> H.Html
showDeckStats player =
  H.table H.! A.class_ "ui very basic collapsing celled table" $ do
    H.thead $
      H.tr $ do
        H.th $ "Cards"
        H.th $ "Money/Card"
        H.th $ "Points/Card"
        H.th $ "% Victory"
        H.th $ "% Treasure"
        H.th $ "% Action"
    H.tbody $
      H.tr $ do
        H.td $ toHtml $ show (length (allCards player))
        H.td $ toHtml $ niceDouble (ratio (moneySum (allCards player)) numCards)
        H.td $ toHtml $ niceDouble (ratio (points player) numCards)
        H.td $ toHtml $ niceDouble (percentage (length (filter isVictory (allCards player))) numCards)
        H.td $ toHtml $ niceDouble (percentage (length (filter isTreasure (allCards player))) numCards)
        H.td $ toHtml $ niceDouble (percentage (length (filter isAction (allCards player))) numCards)
  where
    numCards = length (allCards player)


htmlEndStateSummary :: GameState -> H.Html
htmlEndStateSummary state = do
  H.section H.! A.class_ "state" $ do
    H.div $ do
      H.h4 "Tableau"
      showCards $ Map.map length (piles state)
      when (not (null (trashPile state))) $ do
        H.h4 "Trash"
        showCards $ Map.fromListWith (+) $ map ((,1) . typ) (trashPile state)

  forM_ (zip (reverse $ L.sortOn (points . snd) $ Map.toList (players state)) [0..]) $ \((pid,player),no::Int) -> do
    H.h4 H.! A.class_ (fromString (if no == 0 then "ui top attached header" else "ui attached header")) $
      toHtml $ "Player - " ++ pid ++ ": " ++ show (points player)
    H.div H.! A.class_ "ui attached segment" $ do
      when (Map.findWithDefault 0 VictoryToken (tokens player) > 0) $ do
        H.h5 $ "Victory tokens"
        H.p $ toHtml (show (Map.findWithDefault 0 VictoryToken (tokens player)))

      H.h5 $ "Cards"
      H.p $ showCards (cardListToMap (allCards player))

      H.h5 $ "Stats"
      showDeckStats player


htmlFinished :: GameState -> Stats -> [Info] -> H.Html
htmlFinished state stats infos =
  template "/play" $ do
    column 1 $ ""
    column 10 $ do
      htmlEndStateSummary state

      H.section $ do
        H.h3 "Average Victory Points per Turn"
        svg H.! A.id "avgVictory" H.! A.class_ "chart" $ ""

        H.h3 "Average Money Contents per Turn"
        svg H.! A.id "avgMoney" H.! A.class_ "chart" $ ""

    logsSidebar (map (\(vis,msg) -> (show vis, msg)) infos)

    let ps = Map.keys (players state) in
      H.script $ fromString
                 ("linePlot(\"#avgVictory\",600,400," ++
                  L.intercalate "," (map (\p -> "\"" ++ p ++ "\"," ++ dataToJavaScriptArray ((statAvgVictoryPerTurn stats) Map.! p)) ps) ++
                  ");\n" ++

                  "linePlot(\"#avgMoney\",600,400," ++
                  L.intercalate "," (map (\p -> "\"" ++ p ++ "\"," ++ dataToJavaScriptArray ((statAvgMoneyPerTurn stats) Map.! p)) ps) ++
                  ");\n")

htmlSetupGame :: H.Html
htmlSetupGame =
  template "/game/play" $ do
    column 1 $ ""
    column 14 $ do
      H.div H.! A.id "flash" $ ""

      H.div H.! A.class_ "ui segment" $ do
        H.div H.! A.class_ "ui form" $ do

          H.div H.! A.class_ "inline fields" $ do
            H.label $ "Game type:"
            H.div H.! A.class_ "field" $ do
              H.div H.! A.class_ "ui radio checkbox" $ do
                H.input H.! A.type_ "radio" H.! A.name "gametype" H.! A.value "standard" H.! A.checked "checked"
                H.label "Standard"
            H.div H.! A.class_ "field" $ do
              H.div H.! A.class_ "ui radio checkbox" $ do
                H.input H.! A.type_ "radio" H.! A.name "gametype" H.! A.value "colony"
                H.label "Colony"
            H.div H.! A.class_ "field" $ do
              H.div H.! A.class_ "ui radio checkbox" $ do
                H.input H.! A.type_ "radio" H.! A.name "gametype" H.! A.value "shelters"
                H.label "Shelters"
            H.div H.! A.class_ "field" $ do
              H.div H.! A.class_ "ui radio checkbox" $ do
                H.input H.! A.type_ "radio" H.! A.name "gametype" H.! A.value "colonyShelters"
                H.label "Colony & Shelters"

          forM_ (zip ["Alice", "Bob", "Carol", "Dave"] [1..4]) $ \(name,idx :: Int) -> do
            H.div H.! A.class_ "six wide field" $ do
              H.label $ toHtml ("Player " ++ show idx)
              H.div H.! A.class_ "two fields" $ do
                H.div H.! A.class_ "field" $ do
                  H.input H.! A.type_ "text" H.! A.id (fromString ("playerName" ++ show idx))
                          H.! A.name (fromString ("playerName" ++ show idx)) H.! A.value name
                H.div H.! A.class_ "field" $ do
                  H.select H.! A.class_ "ui dropdown" H.! A.id (fromString ("playerType" ++ show idx))
                           H.! A.name (fromString ("playerType" ++ show idx)) $ do
                    when (idx > 2) $ do
                      H.option H.! A.value "none" H.! A.selected "selected" $ "None"
                    (if idx == 1 then H.option H.! A.value "human" H.! A.selected "selected" $ "Human"
                                 else H.option H.! A.value "human" $ "Human")
                    (if idx == 2 then H.option H.! A.value "bot" H.! A.selected "selected" $ "Bot"
                                 else H.option H.! A.value "bot" $ "Bot")

          H.div $ do
            H.button H.! A.class_ "ui button"
                     H.! A.onclick (fromString ("start();")) $ "Go"
            H.button H.! A.class_ "ui button"
                     H.! A.onclick (fromString ("randomStart([\"" ++ L.intercalate "\",\"" (map cardName $ filter implemented kingdomCards) ++ "\"]);")) $ "Random"


      H.h3 "Choose tableau"

      forM_ [Base, Intrigue, Seaside, Alchemy, Prosperity, Cornucopia, Hinterlands, DarkAges, Guilds, Adventures, Promo] $ \ed -> do
        let cards = filter ((==ed) . edition) kingdomCards
        when (not (null cards)) $ do
          H.section $ do
            H.h3 $ toHtml $ show ed
            H.div H.! A.class_ "ui horizontal selection list" $ do
              forM_ (L.sortOn cardName cards) $ \card ->
                if not (implemented card)
                then
                  H.div H.! A.class_ "item notimplemented" $ do
                    H.img H.! A.style "width: 100px; height: 159px;" H.! A.src (fromString (cardImagePath card))
                    H.div H.! A.class_ "content" $ toHtml (cardName card)
                else
                  H.div H.! A.class_ "cardselection item" H.! A.onclick "$(this).toggleClass('selected');"
                        H.! A.id (fromString (cardName card)) $ do
                    H.img H.! A.style "width: 100px; height: 159px;" H.! A.src (fromString (cardImagePath card))
                    H.div H.! A.class_ "content" $ toHtml (cardName card)


htmlSuggestedTableaus :: [(String, [String])] -> H.Html
htmlSuggestedTableaus tableaus =
  template "/game/suggested" $ do
    column 1 $ ""
    column 14 $ do
      forM_ tableaus $ \(name, cards) -> do
        H.div H.! A.class_ "ui segment" $ do
          H.h3 $ toHtml name
          H.div H.! A.class_ "ui horizontal selection list" $ do
            forM_ cards $ \card -> do
              let c = (lookupCard card)
              H.div H.! A.class_ (if not (implemented c) then "item notimplemented" else "item") $ do
                H.img H.! A.style "width: 100px; height: 159px;" H.! A.src (fromString (cardImagePath c))
                H.div H.! A.class_ "content" $ toHtml (cardName c)
          when (all implemented $ map lookupCard cards) $ do
            H.div $ do
              H.button H.! A.class_ "ui button"
                       H.! A.onclick (fromString ("startGame('standard',[\"" ++ L.intercalate "\",\"" cards ++ "\"]);")) $ "Go"

htmlSimulationForm :: [CardDef] -> Int -> [PlayerId] -> H.Html
htmlSimulationForm tableau numGames players =
  H.div H.! A.class_ "ui segment" $ do
    H.form H.! A.class_ "ui form" H.! A.action "/simulation" $ do

      H.div H.! A.class_ "three wide field" $ do
        H.label "Number of games"
        H.input H.! A.type_ "text" H.! A.name "num" H.! A.value (fromString (show numGames))

      H.div H.! A.class_ "fields" $ do
        forM_ (zip players [1..]) $ \(name,idx :: Int) -> do
          H.div H.! A.class_ "field" $ do
            H.label $ toHtml ("Player " ++ show idx)
            H.select H.! A.class_ "ui selection dropdown" H.! A.name (fromString ("bot" ++ show idx)) $ do
              forM_ (map fst botLibrary) $ \bot -> do
                (if bot == name then H.option H.! A.value (fromString bot) H.! A.selected "selected" $ toHtml bot
                                else H.option H.! A.value (fromString bot) $ toHtml bot)

      H.div H.! A.class_ "field" $ do
        H.label "Tableau"
        H.select H.! A.class_ "ui fluid search dropdown" H.! A.multiple "multiple" H.! A.name "cards" $ do
          forM_ (map cardName kingdomCards) $ \name -> do
            if name `elem` (map cardName tableau)
              then H.option H.! A.value (fromString name) H.! A.selected "selected" $ toHtml name
              else H.option H.! A.value (fromString name) $ toHtml name

      H.div $ do
        H.input H.! A.type_ "submit" H.! A.class_ "ui button" H.! A.name "submit" H.! A.value "Go"
        H.input H.! A.type_ "submit" H.! A.class_ "ui button" H.! A.name "submit" H.! A.value "Sample Game"


htmlSampleGame :: GameState -> [CardDef] -> [Info] -> [PlayerId] -> H.Html
htmlSampleGame state tableau infos bots =
  template "/simulation" $ do
    column 1 $ ""
    column 14 $ do
      htmlSimulationForm tableau 1000 bots

      htmlEndStateSummary state

      H.section $ do
        H.h3 "Log"
        H.div $ do
          forM_  infos $ \(vis,msg) ->
            H.div H.! A.class_ (fromString (if L.isPrefixOf "Turn" msg then "log turn" else "log")) $ do
              H.span H.! A.class_ "player" $ toHtml ("@" ++ show vis ++ " ")
              H.span $ toHtml msg

htmlSimulation :: [CardDef] -> Stats -> [PlayerId] -> H.Html
htmlSimulation tableau stats players =
  template "/simulation" $ do
    column 1 $ ""
    column 14 $ do
      htmlSimulationForm tableau (statNumberOfGames stats) players

      H.div $ do
        H.h3 "Tableau"
        H.div $ do
          forM_ (take 5 $ L.sortBy compareCard tableau) $ \card ->
            H.img H.! A.style "margin: 5px; width: 100px; height: 159px"
                  H.! A.src (fromString (cardImagePath card))
        H.div $ do
          forM_ (drop 5 $ L.sortBy compareCard tableau) $ \card ->
            H.img H.! A.style "margin: 5px; width: 100px; height: 159px"
                  H.! A.src (fromString (cardImagePath card))

        H.h3 "Stats"
        H.table H.! A.class_ "ui very basic collapsing celled table" $ do
          H.thead $
            H.tr $ do
              H.th $ "No Games"
              H.th $ "Avg Turns"
              H.th $ "Median Turns"
              H.th $ "Avg Max Points"
              H.th $ "Avg Min Points"
          H.tbody $
            H.tr $ do
              H.td $ toHtml $ show (statNumberOfGames stats)
              H.td $ toHtml $ niceDouble (statAvgTurnsPerGame stats)
              H.td $ toHtml $ niceDouble (statMedianTurnsPerGame stats)
              H.td $ toHtml $ niceDouble (statAvgMaxPoints stats)
              H.td $ toHtml $ niceDouble (statAvgMinPoints stats)

    column 1 $ ""

    column 1 $ ""
    column 7 $ do
      H.h3 "Winners"
      svg H.! A.id "winners" H.! A.class_ "chart" $ ""

      H.h3 "Turns per Game"
      svg H.! A.id "turns" H.! A.class_ "chart" $ ""

    column 7 $ do
      H.h3 "Average Victory Points per Turn"
      svg H.! A.id "avgVictory" H.! A.class_ "chart" $ ""

      H.h3 "Average Money Contents per Turn"
      svg H.! A.id "avgMoney" H.! A.class_ "chart" $ ""

    H.script $ fromString
               ("barChart(\"#winners\",450,300,percentageData(" ++
                dataToJavaScriptArray (statWinRatio stats) ++
                "));\n" ++

                "barChart(\"#turns\",450,300,percentageData(" ++
                dataToJavaScriptArray (statTurnsPerGame stats) ++
                "));\n" ++

                "linePlot(\"#avgVictory\",450,300," ++
                L.intercalate "," (map (\p -> "\"" ++ p ++ "\"," ++ dataToJavaScriptArray ((statAvgVictoryPerTurn stats) Map.! p)) players) ++
                ");\n" ++

                "linePlot(\"#avgMoney\",450,300," ++
                L.intercalate "," (map (\p -> "\"" ++ p ++ "\"," ++ dataToJavaScriptArray ((statAvgMoneyPerTurn stats) Map.! p)) players) ++
                ");\n")

htmlError :: String -> H.Html
htmlError error =
  template "/error" $ do
    column 1 $ ""
    column 14 $ do
      H.div H.! A.class_ "ui negative message" $ do
        H.div H.! A.class_ "header" $ "Error"
        toHtml error