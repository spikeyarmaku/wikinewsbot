{-# LANGUAGE OverloadedStrings #-}

module Main where

import Webscraper

main :: IO ()
main = scrape >>= putStrLn . show
