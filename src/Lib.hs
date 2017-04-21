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
	mate :: T.Text
  , fell :: Posts
} deriving (Show,G.Generic)

instance FromJSON Data
instance ToJSON Data

askForSub :: IO ()
askForSub = do
	Prelude.putStrLn "Enter a Subreddit"
	subreddit <- getLine
	posts <- getPosts subreddit
	print posts

getPosts :: String -> IO [(Value, Maybe Value, Maybe Value)]
getPosts subreddit = do
	let url = "https://www.reddit.com/r/" ++ subreddit ++ ".json"
	r <- get url
--	let posts = Char8.unpack (r ^. responseBody)
	let f1 = "data" :: T.Text
	let f2 = "children" :: T.Text
	let f3 = "title" :: T.Text
	let dat = r ^. responseBody
	let dec = decode dat :: Maybe Data
	let fil = dec ^.. key f1 . _Array . traverse 
                    . C.to (\o -> ( o ^?! key f2
                                , o ^?  key f1
                                , o ^?  key f3
                                )
                         )
	-- let enc = encode fil
	-- return enc
	return $ fil
--	let fil = posts .key f2 :: (Value -> f Value) -> c
--	return posts
--	return $ Char8.unpack (posts)
--	let enc = encode (posts)
--	let filt = decode (enc)
--	let json = pputJson enc
--	return json
--	let filt = r ^? responseBody . key "url"
--	let posts = Char8.unpack (filt)
--	return filt