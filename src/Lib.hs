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
import Data.Char as Char (isSpace, toUpper)
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
	print posts

getPosts :: String -> IO (Maybe T.Text)
getPosts subreddit = do
	let url = "https://www.reddit.com/r/" Prelude.++ subreddit Prelude.++ ".json"
	r <- get url
    -- Filters for lens (the json keys)
	let f1 = "data" :: T.Text
	let f2 = "children" :: T.Text
	let f3 = "title" :: T.Text
        -- This gets the posts array from the json
        let getPosts = responseBody . key f1 . key f2
	let dat = r ^? responseBody . key f1 . key f2 -- . nth 2 . key f1 . key f3 . _String 
        -- let eachN = dat & each %~ T.toUpper
        let t1 = r ^? getPosts . nth 0 . key f1 . key f3 . _String
        if (t1 == Just "Getting ready for Summer of Haskell 2017")
            then do print "Getting ready for Summer of Haskell 2017"
            else do print "Wot"
        -- case t1 of
        --     just a -> a
        --     Nothing -> "nout"
        let t2 = r ^? getPosts . nth 1 . key f1 . key f3 . _String
        let t3 = r ^? getPosts . nth 2 . key f1 . key f3 . _String
        let t4 = r ^? getPosts . nth 3 . key f1 . key f3 . _String
        let t5 = r ^? getPosts . nth 4 . key f1 . key f3 . _String
        print t1
        print t2
        print t3
        print t4
        print t5
        -- let combined = t1
        return $ t1
