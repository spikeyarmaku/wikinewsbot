module RedditHelper where

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime(), utctDay)

import qualified Data.Text.Lazy as T

import Reddit
import Reddit.Types.Listing
import Reddit.Types.Post

import Types

getRedditPosts :: UTCTime -> Reddit [RedditEntry]
getRedditPosts t = 
  getPosts' (Options Nothing Nothing) New (Just . R . T.toStrict . T.pack $ "wikinews") >>=
    return . map (RedditEntry <$> postID
                              <*> (NewsEntry <$> fromMaybe "" . liftM (T.unpack . T.fromStrict) . flairText
                                             <*> T.unpack . T.fromStrict . title
                                             <*> getUrl . content))
           . filter isPostedByBot . filter isPostedOn . contents
  where
    isPostedOn :: Post -> Bool
    isPostedOn = (== utctDay t) . utctDay . created
    isPostedByBot :: Post -> Bool
    isPostedByBot = (== (Username . T.toStrict . T.pack $ "wikinews-bot")) . author
    getUrl :: PostContent -> String
    getUrl (Link l) = T.unpack . T.fromStrict $ l
    getUrl _        = ""

executeTasks :: [Task] -> Reddit ()
executeTasks = mapM_ executeTask

wikinewsName :: SubredditName
wikinewsName = (R . T.toStrict . T.pack $ "WikiNews")

executeTask :: Task -> Reddit ()
executeTask (PostEntry ne) = do
  liftIO . putStrLn $ "Submitting post: [" ++ newsCategory ne ++ "] " ++ (newsTitle ne)
  newPostId <- submitLink wikinewsName
                          (T.toStrict . T.pack $ newsTitle ne)
                          (T.toStrict . T.pack $ url ne)
  setPostFlair wikinewsName newPostId (T.toStrict . T.pack $ newsCategory ne) (T.toStrict . T.pack . head . words $ newsCategory ne)
executeTask (MarkDeleted re) = do
  liftIO $ putStrLn "Marking deleted"
  setPostFlair wikinewsName (postId re) (T.toStrict . T.pack $ "Deleted") (T.toStrict . T.pack $ "Deleted")
executeTask (ChangeFlair re nc) = do
  liftIO $ putStrLn "Changing flair"
  setPostFlair wikinewsName (postId re) (T.toStrict . T.pack $ nc) (T.toStrict . T.pack . head . words $ nc)