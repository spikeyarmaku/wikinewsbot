{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad        (liftM)
import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.DOM        (parseLBS)
import Text.XML.Cursor      ()

import qualified Data.ByteString.Lazy.Char8 as L

import Data.Time

main :: IO ()
main = do
  pageSource <- liftM parseLBS (todaysLink >>= simpleHttp)
  putStrLn "Hello!"

type Date = (Integer, Int, Int)

currentDate :: IO Date
currentDate = getCurrentTime >>= return . toGregorian . utctDay

wikiPortalLink :: Date -> String
wikiPortalLink (y, m, d) = "https://en.wikipedia.org/wiki/Portal:Current_events/" ++ dateString
  where
    dateString = show y ++ "_" ++ (fst . (!! (m - 1)) . months $ defaultTimeLocale) ++ "_" ++ show d

todaysLink :: IO String
todaysLink = currentDate >>= return . wikiPortalLink