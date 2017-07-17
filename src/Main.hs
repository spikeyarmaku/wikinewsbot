{-# LANGUAGE OverloadedStrings #-}

module Main where

import Webscraper

main :: IO ()
main = scrape >>= putStrLn . show

-- - get all the news articles from wikipedia - let's call it newsEntryList
-- - get all the posts from today from /r/wikinews - let's call it redditEntryList
-- - for each newsEntry in newsEntryList:
--   - get the corresponding entry in redditEntryList by title
--   - if there's a match, discard the current newsEntry
-- - for each newsEntry in newsEntryList:
--   - get the corresponding entry in redditEntryList by url
--   - if there's a match, this redditEntry needs to be edited
--   - otherwise, this newsEntry needs to be posted on reddit
