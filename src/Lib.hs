{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
    ( askForSub
    ) where

import Network.Wreq (get, Response, responseBody)
import Control.Lens as C
import Data.Aeson
import Data.Aeson.Lens
import Data.Map as Map
import qualified Data.Text as T
import Data.ByteString.Lazy.Internal (ByteString)

type Resp = Response (Map String Value)

askForSub :: IO ()
askForSub = do
    Prelude.putStrLn "Enter a Subreddit"
    sub <- getLine
    getPosts sub

openPost :: String -> Response ByteString -> IO ()
openPost sub r = do
    Prelude.putStrLn "Enter a post number to open"
    ln <- getLine
    let n = read ln :: Int
    let f1 = "data" :: T.Text
    let f2 = "children" :: T.Text
    let f3 = "id" :: T.Text
    let getPosts = responseBody . key f1 . key f2
    let id = r ^? getPosts . nth (n-1) . key f1 . key f3 . _String
    getComments sub (strin id)

getPosts :: String -> IO ()
getPosts sub = do
    let url = "https://www.reddit.com/r/" Prelude.++ sub Prelude.++ ".json"
    r <- get url
    printEm sub 0 r

getComments :: String -> String -> IO ()
getComments sub id = do
    let url = "https://www.reddit.com/r/" Prelude.++ sub Prelude.++ "/comments/" Prelude.++ id Prelude.++ ".json"
    r <- get url
    printCom 0 r

printCom :: Int -> Response ByteString -> IO ()
printCom n r
    | n >= 25 = askForSub
    | n < 25 = do
        let f1 = "data" :: T.Text
        let f2 = "children" :: T.Text
        let f3 = "body" :: T.Text
        let getPosts = responseBody . nth 1 . key f1 . key f2
        let p = r ^? getPosts . nth n . key f1 . key f3 . _String
        putStrLn $ id (show (n+1)) Prelude.++ ". " Prelude.++ (strin p)
        printCom (n+1) r

printEm :: String -> Int -> Response ByteString -> IO ()
printEm sub n r
    | n >= 25 = openPost sub r
    | n < 25 = do
        let f1 = "data" :: T.Text
        let f2 = "children" :: T.Text
        let f3 = "title" :: T.Text
        let getPosts = responseBody . key f1 . key f2
        let p = r ^? getPosts . nth n . key f1 . key f3 . _String
        putStrLn $ id (show (n+1)) Prelude.++ ". " Prelude.++ (strin p)
        printEm sub (n+1) r

strin :: Maybe T.Text -> String
strin Nothing = ""
strin (Just x) = (T.unpack x)