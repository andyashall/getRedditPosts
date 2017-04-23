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
	kind :: T.Text
  , data :: Posts
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
	print posts

getPosts :: String -> IO (Maybe Value)
getPosts subreddit = do
	let url = "https://www.reddit.com/r/" ++ subreddit ++ ".json"
	r <- get url
	let f1 = "data" :: T.Text
	let f2 = "children" :: T.Text
	let f3 = "title" :: T.Text
	let dat = r ^? responseBody . key f1 . key f2
    -- let first = dat !!0
	return $ dat