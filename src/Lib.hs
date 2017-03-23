{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( askForSub
    ) where

import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Text
import Data.Map as Map
import Data.ByteString.Lazy.Char8 as Char8
import Data.Char (isSpace)
import Text.HJson.Query

import qualified Data.Text as T

type Resp = Response (Map String Value)

askForSub :: IO ()
askForSub = do
	Prelude.putStrLn "Enter a Subreddit"
	subreddit <- getLine
	posts <- getPosts subreddit
	print posts

getPosts :: String -> IO [Char]
getPosts subreddit = do
	let url = "https://www.reddit.com/r/" ++ subreddit ++ ".json"
	r <- get url
--	let posts = Char8.unpack (r ^. responseBody)
	let f1 = "data" :: T.Text
	let f2 = "children" :: T.Text
	let f3 = "title" :: T.Text
	let fil = r ^. responseBody ^.. key f1 . _Array . traverse 
                    . to (\o -> ( o ^?! key f2
                                , o ^?  key f3
                                )
                         )
	let dat = r ^? responseBody . key f1
	let enc = encode fil
	-- return enc
	return $ Char8.unpack (enc)
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