{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import Data.Either (either)
import Data.Maybe  (mapMaybe)
import Data.Time.Clock (getCurrentTime, UTCTime())
import System.IO (IOMode(ReadMode), withFile, hGetLine, hSetBuffering, stdout, stderr, BufferMode(NoBuffering))
import System.IO.Error (tryIOError)

import qualified Data.Text.Lazy as T

import qualified Reddit as R

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
  readCredential >>= either error (runBot t)

withReddit :: Credential -> R.Reddit a -> IO (Either (R.APIError R.RedditError) a)
withReddit (Credential user pass) =
  R.runRedditWith
    (R.RedditOptions
      True Nothing
      (R.Credentials (T.toStrict . T.pack $ user) (T.toStrict . T.pack $ pass))
      (Just "wikinewsbot 0.1.0.0"))

runBot :: UTCTime -> Credential -> IO ()
runBot t c = do
  wikiList <- scrape t
  ret <- withReddit c $ do
    redditList <- getRedditPosts t
    executeTasks $ createTasks wikiList redditList
  case ret of
    Left err -> print err
    Right x -> return x

readCredential :: IO (Either ErrorMessage Credential)
readCredential =
  withFile  "credential.txt" ReadMode $ \h ->
    tryIOError (hGetLine h) >>= either (return . Left . show) (\user ->
      tryIOError (hGetLine h) >>= either (return . Left . show) (\pass ->
        return . Right $ Credential user pass))

createTasks :: [NewsEntry] -> [RedditEntry] -> [Task]
createTasks ns rs = mapMaybe (flip checkNews $ rs) ns ++ mapMaybe (flip checkRedditPost $ ns) rs

checkNews :: NewsEntry -> [RedditEntry] -> Maybe Task
checkNews ne rs =
  case filter ((== url ne) . url . newsEntry) rs of
    []  -> Just (PostEntry ne)
    x:_ -> if (newsTitle ne == newsTitle (newsEntry x))
              then Nothing
              else Just (EditEntry x ne)

checkRedditPost :: RedditEntry -> [NewsEntry] -> Maybe Task
checkRedditPost re ns =
  if (url . newsEntry $ re) `elem` (map url ns)
    then Nothing
    else Just (MarkDeleted re)

-- news link is in reddit link? -- it was already posted
  -- if yes, compare titles.    -- check for edits
    -- if same, do nothing
    -- if different, edit reddit post
  -- if no, post
-- reddit link is in news link?
  -- if yes, do nothing
  -- if no, mark reddit post as deleted