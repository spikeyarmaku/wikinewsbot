{-# LANGUAGE OverloadedStrings #-}

module Webscraper where

import Control.Monad        (liftM)
import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.DOM        (parseLBS)

import qualified Data.Text     as T
import qualified Data.Map.Lazy as M

import Data.Time
import Text.XML hiding (parseLBS)
import Text.XML.Cursor

type Date = (Integer, Int, Int)
type Title = String
type Url = String
type NewsCategory = String
data NewsEntry = NewsEntry NewsCategory Title Url deriving (Eq, Show)

isAttribute :: String -> String -> Cursor -> Bool
isAttribute attr val c =
  case node c of
    NodeElement el -> case M.lookup (Name (T.pack attr) Nothing Nothing) (elementAttributes el) of
                        Nothing -> False
                        Just x -> x == T.pack val
    _              -> False

elemName :: Cursor -> String
elemName c =
  case node c of
    NodeElement el -> T.unpack . nameLocalName . elementName $ el
    _              -> "N/A"

scrape :: IO [NewsEntry]
scrape = liftM (buildNewsList . cursorNews . fromDocument . parseLBS) (todaysLink >>= simpleHttp)

buildNewsList :: Cursor -> [NewsEntry]
buildNewsList = buildNewsList' "" . child
  where
    buildNewsList' :: NewsCategory -> [Cursor] -> [NewsEntry]
    buildNewsList' _ [] = []
    buildNewsList' newsCat (c:cs) =
      case elemName c of
        "dl" -> buildNewsList' (getNewsCategory c) cs
        "ul" -> getNewsEntries newsCat c ++ buildNewsList' newsCat cs
        _    -> buildNewsList' newsCat cs

getNewsCategory :: Cursor -> String
getNewsCategory c = c $// T.unpack . T.strip . T.concat . content

getNewsEntries :: NewsCategory -> Cursor -> [NewsEntry]
getNewsEntries nc c' = map (\c -> NewsEntry nc (getTitle c) (getLink c)) (getBottomLevelEntries c')

getBottomLevelEntries :: Cursor -> [Cursor]
getBottomLevelEntries c = (c $/ element "li" >=> check isBottomLevel) ++ concatMap getBottomLevelEntries (c $/ element "li" &/ element "ul")

isBottomLevel :: Cursor -> Bool
isBottomLevel c = null (c $/ element "ul")

getTitle :: Cursor -> String
getTitle c = (T.unpack . T.dropAround (`elem` (" \t\r\n\f\v\xa0" :: String)) . T.concat) (concatMap ($.// content) (ditchExternalLinks c))
  where
    ditchExternalLinks :: Cursor -> [Cursor]
    ditchExternalLinks c' = c' $/ check (not . isAttribute "class" "external text")

getLink :: Cursor -> String
getLink c = ((head (c $.// element "a" >=> attributeIs "class" "external text")) $| T.unpack . T.concat . attribute "href")

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