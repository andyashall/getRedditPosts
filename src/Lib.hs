module Lib
    ( askForSub
    ) where

import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (key, _String, nth)
import Data.Aeson.Text (encodeToLazyText)
import Data.Map as Map
import Data.ByteString.Lazy.Char8 as Char8
import Data.Char (isSpace)
import Text.HJson.Query

type Resp = Response (Map String Value)

askForSub :: IO ()
askForSub = do
	Prelude.putStrLn "Enter a Subreddit"
	subreddit <- getLine
	posts <- getPosts subreddit
	Prelude.putStrLn posts

getPosts :: String -> IO [Char]
getPosts subreddit = do
	let url = "https://www.reddit.com/r/" ++ subreddit ++ ".json"
	r <- get url
	let posts = Char8.unpack (r ^. responseBody)
	let enc = encode (posts)
--	let filt = decode (enc)
	let json = pputJson enc
	return json
--	let filt = r ^? responseBody . key "url"
--	let posts = Char8.unpack (filt)
--	return filt