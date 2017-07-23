module RedditHelper where

import Control.Monad (liftM, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime(), utctDay)

import qualified Data.Text.Lazy as T

import Reddit
import Reddit.Types.Listing
import Reddit.Types.Post
import Reddit.Types.Flair

import Types

getRedditPosts :: UTCTime -> Reddit [RedditEntry]
getRedditPosts t = 
  getPosts' (Options Nothing Nothing) New (Just . R . T.toStrict . T.pack $ "wikinews") >>=
    return . map (RedditEntry <$> postID
                              <*> (NewsEntry <$> fromMaybe "" . liftM (T.unpack . T.fromStrict) . flairText
                                             <*> T.unpack . T.fromStrict . title
                                             <*> getUrl . content))
           . filter isPostedOn . contents
  where
    isPostedOn :: Post -> Bool
    isPostedOn = (== utctDay t) . utctDay . created
    getUrl :: PostContent -> String
    getUrl (Link l) = T.unpack . T.fromStrict $ l
    getUrl _        = ""

executeTasks :: [Task] -> Reddit ()
executeTasks = mapM_ executeTask

wikinewsName :: SubredditName
wikinewsName = (R . T.toStrict . T.pack $ "WikiNews")

executeTask :: Task -> Reddit ()
executeTask (PostEntry ne) = do
  liftIO $ putStrLn "Submitting post..."
  newPostId <- submitLink wikinewsName
                          (T.toStrict . T.pack $ newsTitle ne)
                          (T.toStrict . T.pack $ url ne)
  liftIO $ putStrLn $ "PostID: " ++ show newPostId
  flairList <- getFlairList wikinewsName
  unless ((Just . T.toStrict . T.pack $ newsCategory ne) `elem` (map text . contents $ flairList)) $
    addLinkFlair wikinewsName (T.toStrict . T.pack $ "") (T.toStrict . T.pack $ newsCategory ne) False
  setPostFlair wikinewsName newPostId (T.toStrict . T.pack $ newsCategory ne) (T.toStrict . T.pack $ "")
executeTask (EditEntry re ne) = return () -- liftIO $ putStrLn $ "Editing post " ++ (show $ postId re) ++ " to: " ++ show ne
executeTask (MarkDeleted re) = return ()  -- liftIO $ putStrLn $ "Deleting post " ++ show re