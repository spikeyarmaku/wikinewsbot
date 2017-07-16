{-# LANGUAGE OverloadedStrings #-}

module Webscraper where

import Control.Monad        (liftM)
import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.DOM        (parseLBS)
import Text.XML.Cursor      ( fromDocument, Cursor, element, attributeIs, (>=>)
                            , child, content, ($//), ($/), (&/), (&//), cut
                            , (&|), ($|), ($.//))

import qualified Data.Text as T

import Data.Time

type Date = (Integer, Int, Int)
type Title = String
type Url = String
type NewsCategory = String
data NewsEntry = NewsEntry NewsCategory Title Url deriving (Eq)

instance Show NewsEntry where
  show (NewsEntry cat title url) = "[" ++ show cat ++ "] " ++ title ++ " {" ++ url ++ "}"

test :: IO ()
test = do
  cursor <- liftM (fromDocument . parseLBS) (todaysLink >>= simpleHttp)
  putStrLn . show . length . child $ cursorNews cursor

scrape :: IO ()
scrape = liftM (buildNewsList . cursorNews . fromDocument . parseLBS) (todaysLink >>= simpleHttp) >>= putStrLn . show

buildNewsList :: Cursor -> [NewsEntry]
buildNewsList = buildNewsList' "" . child
  where
    buildNewsList' :: NewsCategory -> [Cursor] -> [NewsEntry]
    buildNewsList' _ [] = []
    buildNewsList' lastCategory (c:cs) =
      case c $| element "dl" of
        [] -> case c $| element "ul" &/ element "li" of
          [] -> buildNewsList' lastCategory cs
          _  -> case c $| element "ul" &/ element "li" &/ element "ul" &/ element "li" of
            [] -> NewsEntry lastCategory (c $.// T.unpack . T.strip . T.concat . content) "" : buildNewsList' lastCategory cs
            _  -> buildNewsList' lastCategory ((c $| element "ul" &/ element "li" &/ element "ul") ++ cs)
        _  -> buildNewsList' (c $.// T.unpack . T.strip . T.concat . content) cs

cursorNews :: Cursor -> Cursor
cursorNews cursor = cut . head $ cursor $// element "td" >=> attributeIs "class" "description"

currentDate :: IO Date
currentDate = getCurrentTime >>= return . toGregorian . utctDay

wikiPortalLink :: Date -> String
wikiPortalLink (y, m, d) = "https://en.wikipedia.org/wiki/Portal:Current_events/" ++ dateString
  where
    dateString = show y ++ "_" ++ (fst . (!! (m - 1)) . months $ defaultTimeLocale) ++ "_" ++ show d

todaysLink :: IO String
todaysLink = currentDate >>= return . wikiPortalLink