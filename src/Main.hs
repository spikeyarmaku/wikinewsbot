{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import Data.Either     (either)
import Data.Maybe      (mapMaybe)
import Data.Time.Clock (getCurrentTime, UTCTime())
import System.IO       (IOMode(ReadMode), withFile, hGetLine, hSetBuffering, stdout, stderr, BufferMode(NoBuffering))
import System.IO.Error (tryIOError)

import qualified Data.Text.Lazy    as T
import qualified Text.EditDistance as E
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
  readCredential >>= either error (runBot t)

withReddit :: Credential -> R.Reddit a -> IO (Either (R.APIError R.RedditError) a)
withReddit (Credential user pass) =
  R.runRedditWith
    (R.RedditOptions
      True Nothing
      (R.Credentials (T.toStrict . T.pack $ user) (T.toStrict . T.pack $ pass))
      (Just "useragent goes here..."))

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
    [] -> Just (PostEntry ne)
    _  -> Nothing

checkRedditPost :: RedditEntry -> [NewsEntry] -> Maybe Task
checkRedditPost re ns =
  case filter ((== (url . newsEntry $ re)) . url) ns of
    [] -> Just (MarkDeleted re)
    xs -> if (newsCategory . newsEntry $ re) == (newsCategory . head $ xs)
              then Nothing
              else Just (ChangeFlair re (newsCategory . head $ xs))

class Entry a where
  getTitle :: a -> String
  getUrl :: a -> String

instance Entry RedditEntry where
  getTitle = newsTitle . newsEntry
  getUrl = url . newsEntry

instance Entry NewsEntry where
  getTitle (NewsEntry _ t _) = t
  getUrl (NewsEntry _ _ u) = u

compareEntries :: (Entry a) => a -> a -> Bool
compareEntries x y =
  (getUrl x == getUrl y) || (E.levenshteinDistance E.defaultEditCosts tx ty < (min txl tyl `div` 10))
  where
    tx = getTitle x
    ty = getTitle y
    txl = length tx
    tyl = length ty

-- equalEntries :: 

-- news link is in reddit link? -- it was already posted
  -- if yes, compare titles.    -- check for edits
    -- if same, do nothing
    -- if different, edit reddit post
  -- if no, post
-- reddit link is in news link?
  -- if yes, do nothing
  -- if no, mark reddit post as deleted
