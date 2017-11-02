{-# LANGUAGE OverloadedStrings #-}

module Webscraper where -- (scrape) where

import Control.Monad        (liftM)
import Data.Maybe           (mapMaybe, fromMaybe, fromJust)
import Data.Time.Clock      (UTCTime())
import Network.HTTP.Conduit (simpleHttp)

import qualified Data.ByteString.Lazy  as BL
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import Data.Time

import Types

type Date = (Integer, Int, Int)

scrape :: UTCTime -> IO [NewsEntry]
scrape t = do
  l1 <- (liftM (getNewsAll . deepSegment . T.decodeUtf8 . BL.toStrict) (simpleHttp . todaysLink $ t))
  l2 <- (liftM (getNewsAll . deepSegment . T.decodeUtf8 . BL.toStrict) (simpleHttp . yesterdaysLink $ t))
  return $ l1 ++ l2

truncateTitle :: Int -> T.Text -> T.Text
truncateTitle n t = if T.length t > n then T.take (n - 3) t `T.append` "..." else t

currentDate :: UTCTime -> Date
currentDate = toGregorian . utctDay

yesterday :: UTCTime -> Date
yesterday = toGregorian . addDays (-1) . utctDay

wikiPortalLink :: Date -> String
wikiPortalLink (y, m, d) =
  "https://en.wikipedia.org//w/api.php?action=query&format=json&prop=revisions" ++
  "&rvprop=content&rvcontentformat=text%2Fx-wiki&utf8=1&titles=Portal%3ACurrent_events%2F" ++ dateString
  where
    dateString = show y ++ "_" ++ (fst . (!! (m - 1)) . months $ defaultTimeLocale) ++ "_" ++ show d

todaysLink :: UTCTime -> String
todaysLink = wikiPortalLink . currentDate

yesterdaysLink :: UTCTime -> String
yesterdaysLink = wikiPortalLink . yesterday

getNewsAll :: [[T.Text]] -> [NewsEntry]
getNewsAll = concatMap (\strs -> getNewsBlock (head strs) (tail strs)) . tail

getNewsBlock :: T.Text -> [T.Text] -> [NewsEntry]
getNewsBlock category = mapMaybe (getNewsSingle category)

getNewsSingle :: T.Text -> T.Text -> Maybe NewsEntry
getNewsSingle cat str =
  if ( {-- isPrefixOf "*[[" str && --} T.isSuffixOf "]]" str) ||
     (T.isPrefixOf "<!--" str) ||
     (T.length str < 3)
    then Nothing
    else let titleAndUrl = extractTitle (snd . T.span (== '*') $ str)
             title = T.strip $ fst $ T.break (== '[') titleAndUrl
             link = T.strip $ extractUrl titleAndUrl
         in  Just $ NewsEntry cat (truncateTitle 300 $ title) link

extractTitle :: T.Text -> T.Text
extractTitle "" = ""
extractTitle str =
  let (pre, temp) = T.breakOn "[[" str
      (word, post) = breakOnAfter "]]" temp
  in  if T.null temp
        then pre
        else pre `T.append` decodeMarkup word `T.append` extractTitle post

extractUrl :: T.Text -> T.Text
extractUrl str =
  let (_, temp) = T.break (== '[') str
      (link, _) = T.break (== ']') . fst . T.break (== '(') $ temp
  in T.tail link

breakOnAfter :: T.Text -> T.Text -> (T.Text, T.Text)
breakOnAfter breakAt str =
  let (pre, post) = T.breakOn breakAt str
  in  (pre `T.append` breakAt, fromJust (T.stripPrefix breakAt post))

-- decodes [[whatever|whatever else]] and [[whatever]] style markups, leaves everything else as is
decodeMarkup :: T.Text -> T.Text
decodeMarkup str =
  if (T.isPrefixOf "[[" str && T.isSuffixOf "]]" str)
    then let (pre, post) = T.break (== '|') str
         in  if T.null post
               then fst . T.break (== ']') . fromMaybe pre $ T.stripPrefix "[[" pre
               else T.tail . fst . T.break (== ']') $ post
    else str

deepSegment :: T.Text -> [[T.Text]]
deepSegment = map (T.splitOn "\\n") . T.splitOn ";" . snd . breakOnAfter "\"*\":"
{-
-- ------------------------------ Test stuff ----------------------------------

-- (liftM (getNewsAll . deepSegment . BC8.unpack . BL.toStrict) (simpleHttp . todaysLink $ t))
str :: IO T.Text
str = do
  x <- simpleHttp $ wikiPortalLink (2017, 11, 02)
  return $ T.decodeUtf8 . BL.toStrict $ x
  -- putStrLn . printArray2 $ [["test1", "test2"], ["test3", "test4"], ["test5", "test6", "test7"]]
  -- print . getNewsAll . deepSegment . BC8.unpack . BL.toStrict $ str

test1 :: IO ()
test1 = do
  x <- str
  putStrLn $ printNews $ getNewsAll $ deepSegment x

printArray2 :: [[T.Text]] -> T.Text
printArray2 [] = ""
printArray2 (x:xs) = printArray1 x `T.append` "\n---\n" `T.append` printArray2 xs

printArray1 :: [T.Text] -> T.Text
printArray1 [] = ""
printArray1 (x:xs) = "> " `T.append` x `T.append` "\n" `T.append` printArray1 xs

printNews :: Show a => [a] -> String
printNews [] = ""
printNews (x:xs) = "> " ++ show x ++ "\n" ++ printNews xs
-}