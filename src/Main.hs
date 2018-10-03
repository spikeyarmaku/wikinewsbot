{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import Control.Monad      (forM_)
import Data.Either        (either)
import Data.Maybe         (mapMaybe)
import Data.Time.Clock    (getCurrentTime, UTCTime())
import System.Environment (getArgs)
import System.IO          (hSetBuffering, stdout, stderr, BufferMode(NoBuffering))

import qualified Data.Text.Lazy    as T
import qualified Reddit            as R

import Webscraper
import RedditHelper
import Types

type Username = String
type Password = String
data Credential = Credential Username Password deriving (Eq, Show)

type ErrorMessage = String

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  t <- getCurrentTime
  readCredential >>= either putStrLn (runBot t)

withReddit :: Credential -> R.Reddit a -> IO (Either (R.APIError R.RedditError) a)
withReddit (Credential user pass) =
  R.runRedditWith
    (R.RedditOptions
      True Nothing
      (R.Credentials (T.toStrict . T.pack $ user) (T.toStrict . T.pack $ pass))
      (Just "Bot:identifier:0.1.0 (by creator)"))

runBot :: UTCTime -> Credential -> IO ()
runBot t c = do
  putStr "Reading news from Wikipedia..."
  wikiList <- scrape t
  print wikiList
  putStrLn "done."
  putStr "Reading posts from Reddit and creating tasks..."
  withReddit c (getRedditPosts t >>= \redditList -> return $ createTasks wikiList redditList)
    >>= \case
      Left err -> print err
      Right tasks -> do
        putStrLn "done."
        putStrLn "Executing tasks..."
        forM_ tasks $ \task -> do
          ret <- withReddit c $ executeTask task
          print ret

readCredential :: IO (Either ErrorMessage Credential)
readCredential = do
  args <- getArgs
  case args of
    username:password:_ -> return $ Right (Credential username password)
    _                   -> return $ Left "No credential provided. Usage: wikinewsbot <username> <password>"

createTasks :: [NewsEntry] -> [RedditEntry] -> [Task]
createTasks ns rs = mapMaybe (flip checkNews $ rs) ns ++ mapMaybe (flip checkRedditPost $ ns) rs

checkNews :: NewsEntry -> [RedditEntry] -> Maybe Task
checkNews ne rs =
  case filter ((== url ne) . url . newsEntry) rs of
    [] -> Just (PostEntry ne)
    _  -> Nothing

checkRedditPost :: RedditEntry -> [NewsEntry] -> Maybe Task
checkRedditPost re ns =
  case filter ((== (url . newsEntry $ re)) . url) ns of
    [] -> Just (MarkDeleted re)
    xs -> if (newsCategory . newsEntry $ re) == (newsCategory . head $ xs)
              then Nothing
              else Just (ChangeFlair re (newsCategory . head $ xs))
