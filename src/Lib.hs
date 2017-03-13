module Lib
    ( askForSub
    ) where

import Network.Wreq
import Control.Lens
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Map as Map
import Data.ByteString.Lazy.Char8 as Char8
import Data.Char (isSpace)

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
	return $ Char8.unpack (r ^. responseBody)
