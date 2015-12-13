{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Dominion.Web.Pages where

import Dominion.Model
import Dominion.Cards
import Dominion.Stats
import Dominion

import Control.Monad (forM_, when)

import qualified Data.ByteString.Char8 as C8
import Data.Char (toLower)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.String (fromString)

import Snap.Http.Server (snapServerVersion)

import Text.Blaze.Html (toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (MarkupM(Parent))


-- Utilities

cardImagePath :: Card -> String
cardImagePath card = "/static/images/cards/"
  ++ filter (not . (=='\'')) (map (\c -> if c == ' ' then '_' else toLower c) (cardName card))
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
  H.link H.! A.href "/static/libs/semanticui/semantic.min.css" H.! A.rel "stylesheet" H.! A.type_ "text/css"
  H.script H.! A.src "/static/js/jquery-2.1.4.min.js" $ ""
  H.script H.! A.src "/static/libs/semanticui/semantic.min.js" $ ""
  H.script H.! A.src "/static/js/play.js" $ ""
  H.script H.! A.src "/static/js/d3.v3.min.js" H.! A.charset "utf-8" $ ""
  H.script H.! A.src "/static/js/graph.js" H.! A.charset "utf-8" $ ""

template :: String -> H.Html -> H.Html
template link inner =
  H.html $ do
    htmlHeader
    H.body $ do
      H.div H.! A.class_ "ui inverted segment" $ do
        H.div H.! A.class_ "ui inverted secondary menu" $ do
          forM_ [("Home","/"), ("Play","/game/play"), ("Simulation","/simulation")] $ \(label,href) ->
            H.a H.! A.class_ (if href == link then "active item" else "item") H.! A.href (fromString href) $ label
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
    H.button H.! A.class_ "choice-button" H.! A.onclick "pass();" $ "Pass"

decisionHtml (ChooseToUse effect _) =
  H.div $ do
    toHtml $ "Use " ++ show effect
    H.button H.! A.class_ "choice-button" H.! A.onclick "choose('true');" $ "Yes"
    H.button H.! A.class_ "choice-button" H.! A.onclick "choose('false');" $ "No"

decisionHtml (ChooseToReact card trigger _) =
  H.div $ do
    toHtml $ "Use " ++ show card ++ "'s reaction to respond to " ++ show trigger
    H.button H.! A.class_ "choice-button" H.! A.onclick "choose('true');" $ "Yes"
    H.button H.! A.class_ "choice-button" H.! A.onclick "choose('false');" $ "No"

decisionHtml (ChooseCard effect choices _) =
  H.div $ do
    toHtml $ "Choose a cards to " ++ show effect ++ ":"
    H.div H.! A.id "choices" $ do
      forM_ choices $ \card -> do
        H.input H.! A.type_ "image"
                H.! A.onclick (fromString ("choose(\""++ cardName card ++ "\",1,1);"))
                H.! A.style "margin: 5px; width: 100px; height: 159px"
                H.! A.src (fromString (cardImagePath card))

decisionHtml (ChooseCards effect choices (lo,hi) _) =
  H.div $ do
    toHtml $ "Choose between " ++ show lo ++ " and " ++ show hi ++ " cards to " ++ show effect ++ ":"
    H.div H.! A.id "choices" $ do
      forM_ choices $ \card -> do
        H.input H.! A.type_ "image"
                H.! A.name (fromString (cardName card))
                H.! A.class_ "checkbox"
                H.! A.style "margin: 5px; width: 100px; height: 159px"
                H.! A.src (fromString (cardImagePath card))
    if length choices <= hi
      then H.button H.! A.class_ "choice-button"
                    H.! A.onclick (fromString ("choose(\""++ L.intercalate "," (map cardName choices) ++ "\");"))
                    $ "All"
      else return ()
    H.button H.! A.class_ "choice-button"
             H.! A.onclick (fromString ("choices('choices'," ++ show lo ++ "," ++ show hi ++ ");")) $ "Go"

decisionHtml (ChooseEffects no effects _) =
  H.div $ do
    H.p $ toHtml $ "Choose " ++ show no ++ ":"
    H.div H.! A.id "choices" $ do
      forM_ (zip [0..] effects) $ \(no::Int,effect) ->
        H.div H.! A.class_ "ui checkbox" $ do
          H.input H.! A.type_ "checkbox" H.! A.name (fromString (show no))
          H.label $ toHtml (show effect)
    H.button H.! A.class_ "choice-button"
             H.! A.onclick (fromString ("choices('choices'," ++ show no ++ "," ++ show no ++ ");")) $ "Go"


compareCard :: Card -> Card -> Ordering
compareCard c1 c2 =
  compare (moneyCost (cost NullModifier c1)) (moneyCost (cost NullModifier c2))
  `mappend` compare (potionCost (cost NullModifier c1)) (potionCost (cost NullModifier c2))
  `mappend` compare (cardName c1) (cardName c2)


showCards cards =
  forM_ (L.sortBy (\(c1,_) (c2,_) -> compareCard c1 c2) (Map.toList cards))
    $ \(card,num) ->
      H.div H.! A.class_ "imageoverlay" $ do
        H.img H.! A.style "margin: 5px; width: 100px; height: 159px"
              H.! A.src (fromString (cardImagePath card))
        H.h3 H.! A.class_ "overlay" $ toHtml (show num)

cardListToMap :: [Card] -> Map.Map Card Int
cardListToMap cards =
  Map.fromListWith (+) $ map (,1) cards


htmlDecision :: PlayerId -> (GameState,[Info],Decision) -> H.Html
htmlDecision _ (state,infos,decision) =
  template "/play" $ do
    H.div H.! A.class_ "ten wide column" $ do
      H.div H.! A.id "flash" $ ""
      H.section H.! A.class_ "decision" $ do
        H.h3 "Decision"
        decisionHtml decision

      H.section H.! A.class_ "state" $ do
        H.div $ do
          H.h4 "Turn: "
          H.span $ toHtml $ "Turn: " ++ show (turnNo state) ++ ", "
          H.span $ toHtml $ "Actions: " ++ show (actions (turn state)) ++ ", "
          H.span $ toHtml $ "Buys: " ++ show (buys (turn state)) ++ ", "
          H.span $ toHtml $ "Money: " ++ show (money (turn state))
          when (0 < potions (turn state)) $ H.span $ toHtml $ ", Potions: " ++ show (potions (turn state))

        H.div $ do
          H.h4 "Tableau:"
          showCards (piles state)

        forM_ (Map.toAscList (players state)) $ \(pid,player) ->
          H.div $ do
            H.h4 $ toHtml $ "Player - " ++ pid
            when (not (null (deck player))) $ do
              H.h5 "Deck: "
              showCards (cardListToMap (deck player))
            when (not (null (discardPile player))) $ do
              H.h5 "Discard: "
              showCards (cardListToMap (discardPile player))
            when (not (null (hand player))) $ do
              H.h5 "Hand: "
              showCards (cardListToMap (hand player))
            when (not (null (inPlay player))) $ do
              H.h5 "InPlay: "
              showCards (cardListToMap (inPlay player))

    H.div H.! A.class_ "six wide column" $ do
      H.section H.! A.class_ "logs" $ do
        H.h3 "Game Log"
        H.div $ do
          when (length infos > 20) $ do
            H.div H.! A.class_ "log" $ do
              H.span "[...]"
          forM_  (drop (max 0 (length infos - 20)) infos) $ \(vis,msg) ->
            H.div H.! A.class_ "log" $ do
              H.span H.! A.class_ "player" $ toHtml ("@" ++ show vis ++ " ")
              H.span $ toHtml msg


htmlFinished :: GameState -> [Info] -> H.Html
htmlFinished state infos =
  template "/play" $ do
    H.div H.! A.class_ "ten wide column" $ do
      H.section H.! A.class_ "state" $ do
        H.div $ do
          H.h4 "Tableau:"
          showCards (piles state)

        forM_ (reverse $ L.sortOn (points . snd) $ Map.toList (players state)) $ \(pid,player) ->
          H.section $ do
            H.h4 $ toHtml $ "Player - " ++ pid ++ ": " ++ show (points player)
            H.h5 "Cards: "
            showCards (cardListToMap (allCards player))

    H.div H.! A.class_ "six wide column" $ do
      H.section H.! A.class_ "logs" $ do
        H.h3 "Game Log"
        H.div $ do
          forM_  infos $ \(vis,msg) ->
            H.div H.! A.class_ "log" $ do
              H.span H.! A.class_ "player" $ toHtml ("@" ++ show vis ++ " ")
              H.span $ toHtml msg

htmlSetupGame :: H.Html
htmlSetupGame =
  template "/game/play" $ do
    H.div H.! A.class_ "one wide column" $ ""
    H.div H.! A.class_ "fourteen wide column" $ do
      H.div H.! A.id "flash" $ ""
      H.h3 "Choose tableau"

      forM_ [Base, Intrigue, Seaside, Alchemy, Prosperity, Cornucopia, Hinterlands, DarkAges, Guilds, Adventures, Promo] $ \ed -> do
        let cards = filter ((==ed) . edition) kingdomCards
        when (not (null cards)) $ do
          H.section $ do
            H.h3 $ toHtml $ show ed
            H.div H.! A.class_ "ui horizontal selection list" $ do
              forM_ (L.sortOn cardName cards) $ \card ->
                H.div H.! A.class_ "item" H.! A.onclick "$(this).toggleClass('selected');"
                      H.! A.id (fromString (cardName card)) $ do
                  H.img H.! A.style "width: 100px; height: 159px;" H.! A.src (fromString (cardImagePath card))
                  H.div H.! A.class_ "content" $ toHtml (cardName card)

      H.div $ do
        H.button H.! A.class_ "choice-button"
                 H.! A.onclick (fromString ("start();")) $ "Go"
        H.button H.! A.class_ "choice-button"
                 H.! A.onclick (fromString ("randomStart([\"" ++ L.intercalate "\",\"" (map cardName kingdomCards) ++ "\"]);")) $ "Random"


htmlSimulation :: [Card] -> Stats -> H.Html
htmlSimulation tableau stats =
  template "/simulation" $ do
    H.div H.! A.class_ "one wide column" $ ""
    H.div H.! A.class_ "fourteen wide column" $ do
      H.h3 "Tableau"

      H.div $ do
        forM_ (L.sortBy compareCard tableau) $ \card ->
          H.img H.! A.style "margin: 5px; width: 150px; height: 238px"
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

                  "scatterPlot(\"#avgVictory\",600,400,"++
                  dataToJavaScriptArray ((statAvgVictoryPerTurn stats) Map.! "Alice") ++ "," ++
                  dataToJavaScriptArray ((statAvgVictoryPerTurn stats) Map.! "Bob") ++
                  ");\n"
                  )