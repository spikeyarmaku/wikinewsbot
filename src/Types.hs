module Types where

import Data.Time.Clock (UTCTime())

import Reddit

type Title = String
type Url = String
type NewsCategory = String
data NewsEntry = NewsEntry { newsCategory :: NewsCategory
                           , newsTitle :: Title
                           , url :: Url }
                           deriving (Eq, Show)
                           
data RedditEntry = RedditEntry { postId :: PostID
                               , newsEntry :: NewsEntry }
                               deriving (Eq, Show)

data Settings = Settings {currentTime :: UTCTime}

type WikiBot a = RedditT IO a

data Task = PostEntry NewsEntry
          | MarkDeleted RedditEntry
          | ChangeFlair RedditEntry NewsCategory
          deriving (Eq, Show)