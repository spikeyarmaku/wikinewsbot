{-# LANGUAGE OverloadedStrings #-}

module RedditHelper where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime(), utctDay)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)

import qualified Data.Text as T

import Reddit
import Reddit.Types.Listing
import Reddit.Types.Post

import Types

getRedditPosts :: UTCTime -> Reddit [RedditEntry]
getRedditPosts t = 
  getPosts' (Options Nothing Nothing) New (Just . R . T.pack $ "wikinews") >>=
    return . map (RedditEntry <$> postID
                              <*> (NewsEntry <$> fromMaybe "" . flairText
                                             <*> title
                                             <*> getUrl . content))
           . filter isPostedByBot . filter isPostedOn . contents
  where
    isPostedOn :: Post -> Bool
    isPostedOn = (== utctDay t) . utctDay . created
    isPostedByBot :: Post -> Bool
    isPostedByBot = (== (Username . T.pack $ "wikinews-bot")) . author
    getUrl :: PostContent -> T.Text
    getUrl (Link l) = toStrict . toLazyText . htmlEncodedText $ l
    getUrl _        = ""

executeTasks :: [Task] -> Reddit ()
executeTasks = mapM_ executeTask

wikinewsName :: SubredditName
wikinewsName = (R . T.pack $ "WikiNews")

executeTask :: Task -> Reddit ()
executeTask (PostEntry ne) = do
  liftIO . putStrLn $ "Submitting post: [" ++ show (newsCategory ne) ++ "] " ++ show (newsTitle ne)
  newPostId <- submitLink wikinewsName (newsTitle ne) (url ne)
  setPostFlair wikinewsName newPostId (newsCategory ne) (head . T.words $ newsCategory ne)
executeTask (MarkDeleted re) = do
  liftIO $ putStrLn "Marking deleted"
  setPostFlair wikinewsName (postId re) (T.pack $ "Deleted") (T.pack $ "Deleted")
executeTask (ChangeFlair re nc) = do
  liftIO $ putStrLn "Changing flair"
  setPostFlair wikinewsName (postId re) nc (head . T.words $ nc)