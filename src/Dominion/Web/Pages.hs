{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Dominion.Web.Pages where

import Dominion.Model
import Dominion.Cards
import Dominion.Stats
import Dominion
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

htmlHomePage :: H.Html
htmlHomePage =
  template "/" $ do
    H.div H.! A.class_ "one wide column" $ ""
    H.div H.! A.class_ "fourteen wide column" $ do
      H.section $ do
        H.h3 "Dominion Play Server"
        H.p "A small experimental Dominion server for both bot and human play."


decisionHtml :: Decision -> H.Html
decisionHtml (Optional inner _) =
  H.div $ do
    decisionHtml inner
    H.button H.! A.class_ "ui button" H.! A.onclick "pass();" $ "Pass"

decisionHtml (ChooseNumber effect (lo,hi) _) =
  H.div $ do
    toHtml $ "Choose how many to use for " ++ show effect
    forM_ [lo..hi] $ \n ->
      H.button H.! A.class_ "ui button" H.! A.onclick (fromString ("choose('" ++ show n ++ "');")) $ fromString (show n)

decisionHtml (ChooseToUse effect _) =
  H.div $ do
    toHtml $ "Use " ++ show effect
    H.button H.! A.class_ "ui button" H.! A.onclick "choose('true');" $ "Yes"
    H.button H.! A.class_ "ui button" H.! A.onclick "choose('false');" $ "No"

decisionHtml (ChooseToReact card trigger _) =
  H.div $ do
    toHtml $ "Use " ++ show card ++ "'s reaction to respond to " ++ show trigger
    H.button H.! A.class_ "ui button" H.! A.onclick "choose('true');" $ "Yes"
    H.button H.! A.class_ "ui button" H.! A.onclick "choose('false');" $ "No"

decisionHtml (ChooseCard effect choices _) =
  H.div $ do
    toHtml $ "Choose a cards to " ++ show effect ++ ":"
    H.div H.! A.id "choices" $ do
      forM_ choices $ \card -> do
        H.input H.! A.type_ "image"
                H.! A.onclick (fromString ("choose(\""++ cardName (typ card) ++ "\",1,1);"))
                H.! A.style "margin: 5px; width: 100px; height: 159px"
                H.! A.src (fromString (cardImagePath (typ card)))

decisionHtml (ChooseCards effect choices (lo,hi) _) =
  H.div $ do
    toHtml $ "Choose between " ++ show lo ++ " and " ++ show hi ++ " cards to " ++ show effect ++ ":"
    H.div H.! A.id "choices" $ do
      forM_ choices $ \card -> do
        H.input H.! A.type_ "image"
                H.! A.name (fromString (cardName (typ card)))
                H.! A.class_ "cardbox"
                H.! A.style "margin: 5px; width: 100px; height: 159px"
                H.! A.src (fromString (cardImagePath (typ card)))
    if length choices <= hi
      then H.button H.! A.class_ "ui button"
                    H.! A.onclick (fromString ("choose(\""++ L.intercalate "," (map (cardName . typ) choices) ++ "\");"))
                    $ "All"
      else return ()
    H.button H.! A.class_ "ui button"
             H.! A.onclick (fromString ("choices('choices'," ++ show lo ++ "," ++ show hi ++ ");")) $ "Go"

decisionHtml (ChooseEffects no effects _) =
  H.div $ do
    H.p $ toHtml $ "Choose " ++ show no ++ ":"
    H.div H.! A.id "choices" $ do
      forM_ (zip [0..] effects) $ \(no::Int,effect) ->
        H.div H.! A.class_ "ui checkbox" $ do
          H.input H.! A.type_ "checkbox" H.! A.name (fromString (show no))
          H.label $ toHtml (show effect)
    H.button H.! A.class_ "ui button"
             H.! A.onclick (fromString ("choices('choices'," ++ show no ++ "," ++ show no ++ ");")) $ "Go"


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
cardListToMap cards =
  Map.fromListWith (+) $ map ((,1) . typ) cards


logsSidebar :: [(String,String)] -> H.Html
logsSidebar infos =
  H.div H.! A.class_ "six wide column" $ do
    H.section H.! A.class_ "logs" $ do
      H.h3 "Game Log"
      H.div $ do
        forM_  infos $ \(vis,msg) ->
          H.div H.! A.class_ "log" $ do
            H.span H.! A.class_ "player" $ toHtml ("@" ++ vis ++ " ")
            H.span $ toHtml msg


htmlDecision :: PlayerId -> (GameState,[Info],Decision) -> H.Html
htmlDecision p (state,infos,decision) =
  template "/play" $ do
    H.div H.! A.class_ "ten wide column" $ do
      H.div H.! A.id "flash" $ ""
      H.section H.! A.class_ "decision" $ do
        H.h3 "Decision"
        decisionHtml decision

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

      forM_ (Map.toAscList (players state)) $ \(pid,player) ->
        H.section $ do
          H.h4 $ toHtml $ "Player - " ++ pid
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
      then ("", "[...]"):map (\(vis,msg) -> ("@" ++ show vis, msg)) (drop (max 0 (length infos - 20)) infos)
      else map (\(vis,msg) -> ("@" ++ show vis, msg)) infos


htmlFinished :: GameState -> [Info] -> H.Html
htmlFinished state infos =
  template "/play" $ do
    H.div H.! A.class_ "ten wide column" $ do
      H.section H.! A.class_ "state" $ do
        H.div $ do
          H.h4 "Tableau:"
          showCards $ Map.map length (piles state)

      forM_ (reverse $ L.sortOn (points . snd) $ Map.toList (players state)) $ \(pid,player) ->
        H.section $ do
          H.h4 $ toHtml $ "Player - " ++ pid ++ ": " ++ show (points player)
          when (Map.findWithDefault 0 VictoryToken (tokens player) > 0) $ do
            H.p $ toHtml ("Victory tokens: " ++ show (Map.findWithDefault 0 VictoryToken (tokens player)))
          H.h5 "Cards: "
          showCards (cardListToMap (allCards player))

    logsSidebar (map (\(vis,msg) -> ("@" ++ show vis, msg)) infos)


htmlSetupGame :: H.Html
htmlSetupGame =
  template "/game/play" $ do
    H.div H.! A.class_ "one wide column" $ ""
    H.div H.! A.class_ "fourteen wide column" $ do
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
                  H.div H.! A.class_ "item" H.! A.onclick "$(this).toggleClass('selected');"
                        H.! A.id (fromString (cardName card)) $ do
                    H.img H.! A.style "width: 100px; height: 159px;" H.! A.src (fromString (cardImagePath card))
                    H.div H.! A.class_ "content" $ toHtml (cardName card)


htmlSuggestedTableaus :: [(String, [String])] -> H.Html
htmlSuggestedTableaus tableaus =
  template "/game/suggested" $ do
    H.div H.! A.class_ "one wide column" $ ""

    H.div H.! A.class_ "fourteen wide column" $ do
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


htmlSimulation :: [CardDef] -> Stats -> [PlayerId] -> H.Html
htmlSimulation tableau stats players =
  template "/simulation" $ do
    H.div H.! A.class_ "one wide column" $ ""
    H.div H.! A.class_ "fourteen wide column" $ do
      H.div H.! A.class_ "ui segment" $ do
        H.form H.! A.class_ "ui form" H.! A.action "/simulation" $ do

          H.div H.! A.class_ "three wide field" $ do
            H.label "Number of games"
            H.input H.! A.type_ "text" H.! A.name "num" H.! A.value (fromString (show (statNumberOfGames stats)))

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
            H.input H.! A.type_ "submit" H.! A.class_ "ui button" H.! A.value "Go"

      H.section $ do
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
        H.pre $ toHtml $ showStats stats

        H.h3 "Winners"
        svg H.! A.id "winners" H.! A.class_ "chart" $ ""

        H.h3 "Turns per Game"
        svg H.! A.id "turns" H.! A.class_ "chart" $ ""

        H.h3 "Average Victory Points per Turn"
        svg H.! A.id "avgVictory" H.! A.class_ "chart" $ ""

        H.script $ fromString
                   ("barChart(\"#winners\",300,200,percentageData(" ++
                    dataToJavaScriptArray (statWinRatio stats) ++
                    "));\n" ++

                    "barChart(\"#turns\",600,400,percentageData(" ++
                    dataToJavaScriptArray (statTurnsPerGame stats) ++
                    "));\n" ++

                    "scatterPlot(\"#avgVictory\",600,400," ++
                    L.intercalate "," (map (\p -> dataToJavaScriptArray ((statAvgVictoryPerTurn stats) Map.! p)) players) ++
                    ");\n"
                    )