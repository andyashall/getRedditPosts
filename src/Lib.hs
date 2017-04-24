{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
    ( askForSub
    ) where

import GHC.Generics as G
import Network.Wreq
import Control.Lens as C
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Text
import Data.Map as Map
import Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.ByteString.Lazy.Char8 as Char8
import Data.Char (isSpace)
import Text.HJson.Query

import qualified Data.Text as T

type Resp = Response (Map String Value)

data Post = Post {
	hello :: T.Text
  , fella :: Object
} deriving (Show,G.Generic)

data Posts = Posts {
	after :: T.Text
  , before :: T.Text
  , children :: Post
  , modhash :: T.Text
} deriving (Show,G.Generic)

data Data = Data {
	mate :: T.Text
  , fell :: Posts
} deriving (Show,G.Generic)

instance FromJSON Data
instance ToJSON Data

instance FromJSON Posts
instance ToJSON Posts

instance FromJSON Post
instance ToJSON Post

askForSub :: IO ()
askForSub = do
	Prelude.putStrLn "Enter a Subreddit"
	subreddit <- getLine
	posts <- getPosts subreddit
        -- firs <- makeArray posts
	print posts

getPosts :: String -> IO (Maybe T.Text)
getPosts subreddit = do
	let url = "https://www.reddit.com/r/" Prelude.++ subreddit Prelude.++ ".json"
	r <- get url
	let f1 = "data" :: T.Text
	let f2 = "children" :: T.Text
	let f3 = "title" :: T.Text
	let dat = r ^? responseBody . key f1 . key f2 . _Array . traverse . key f1 . key f3 . _String 
        return $ dat
        -- titles <- HM.lookup "title" dat
        -- case titles of
        --     Just x -> return x
        --     Nothing -> return "No titles"

-- makeArray :: Maybe Value -> [Value]
-- makeArray dat = do
--     let fir = dat !!0
--     return fir