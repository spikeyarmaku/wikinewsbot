{-# LANGUAGE OverloadedStrings #-}

module Webscraper where

import Control.Monad        (liftM)
import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.DOM        (parseLBS)
import Text.XML.Cursor      ( fromDocument, Cursor, element, attributeIs, (>=>)
                            , child, content, ($//), ($/), (&/), (&//), cut
                            , (&|), ($|), ($.//), attribute)

import qualified Data.Text as T

import Data.Time

type Date = (Integer, Int, Int)
type Title = String
type Url = String
type NewsCategory = String
data NewsEntry = NewsEntry NewsCategory Title Url deriving (Eq)

instance Show NewsEntry where
  show (NewsEntry cat title url) = "[" ++ cat ++ "] " ++ title ++ " {" ++ url ++ "}"

test :: IO ()
test = do
  cursor <- liftM (fromDocument . parseLBS) (todaysLink >>= simpleHttp)
  putStrLn . show . filter (not . null) . map ($| element "ul" &/ element "li" &/ element "a" &| attribute "href") . child $ cursorNews cursor

scrape :: IO ()
scrape = liftM (buildNewsList . cursorNews . fromDocument . parseLBS) (todaysLink >>= simpleHttp) >>= putStrLn . show

buildNewsList :: Cursor -> [NewsEntry]
buildNewsList = buildNewsList' "" . child
  where
    buildNewsList' :: NewsCategory -> [Cursor] -> [NewsEntry]
    buildNewsList' _ [] = []
    buildNewsList' lastCategory (c:cs) =
      let firstLevel = element "ul" &/ element "li"
          secondLevel = firstLevel &/ firstLevel
      in  case c $| element "dl" of
            [] -> case c $| firstLevel of
              [] -> buildNewsList' lastCategory cs
              _  -> case c $| secondLevel of
                [] -> NewsEntry lastCategory
                                (c $.// T.unpack . T.dropAround (`elem` ("\t\r\n\f\v\xa0" :: String)) . T.concat . content)
                                ((head (reverse (c $| firstLevel &/ element "a"))) $| T.unpack . T.concat . attribute "href")
                                : buildNewsList' lastCategory cs
                _  -> buildNewsList' lastCategory ((c $| firstLevel &/ element "ul") ++ cs)
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