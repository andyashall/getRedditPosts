{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
    ( askForSub
    ) where

import Network.Wreq
import Control.Lens as C
import Data.Aeson
import Data.Aeson.Lens
import Data.Map as Map
import qualified Data.Text as T
import Data.ByteString.Lazy.Internal

type Resp = Response (Map String Value)

askForSub :: IO ()
askForSub = do
    Prelude.putStrLn "Enter a Subreddit"
    subreddit <- getLine
    getPosts subreddit

getPosts :: String -> IO ()
getPosts subreddit = do
    let url = "https://www.reddit.com/r/" Prelude.++ subreddit Prelude.++ ".json"
    r <- get url
    printEm 0 r

printEm :: Int -> Response ByteString -> IO ()
printEm n r
    | n >= 10 = askForSub
    | n < 10 = do
        let f1 = "data" :: T.Text
        let f2 = "children" :: T.Text
        let f3 = "title" :: T.Text
        let getPosts = responseBody . key f1 . key f2
        let p = r ^? getPosts . nth n . key f1 . key f3 . _String
        print $ show (n+1) Prelude.++ ". " Prelude.++ (strin p)
        printEm (n+1) r

strin :: Maybe T.Text -> String
strin Nothing = ""
strin (Just x) = (T.unpack x)