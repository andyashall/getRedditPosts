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
    putStrLn "Enter a Subreddit"
    sub <- getLine
    getPosts sub

openPost :: String -> Response ByteString -> IO ()
openPost sub r = do
    putStrLn "Enter a post number to open"
    ln <- getLine
    let n = read ln :: Int
    let f1 = "data" :: T.Text
    let getPosts = responseBody . key f1 . key "children"
    let id = r ^? getPosts . nth (n-1) . key f1 . key "id" . _String
    getComments sub (strin id)

getPosts :: String -> IO ()
getPosts sub = do
    let url = "https://www.reddit.com/r/" ++ sub ++ ".json"
    r <- get url
    printEm sub 0 r

getComments :: String -> String -> IO ()
getComments sub id = do
    let url = "https://www.reddit.com/r/" ++ sub ++ "/comments/" ++ id ++ ".json"
    r <- get url
    parseComments 0 r

parseComments :: Int -> Response ByteString -> IO ()
parseComments n r
    | n >= 25 = askForSub
    | n < 25 = do
        let f1 = "data" :: T.Text
        let getPosts = responseBody . nth 1 . key f1 . key "children"
        let p = r ^? getPosts . nth n . key f1 . key "body" . _String
        let s = (strin p)
        printComments n r s

printEm :: String -> Int -> Response ByteString -> IO ()
printEm sub n r
    | n >= 25 = openPost sub r
    | n < 25 = do
        let f1 = "data" :: T.Text
        let getPosts = responseBody . key f1 . key "children"
        let p = r ^? getPosts . nth n . key f1 . key "title" . _String
        putStrLn $ id (show (n+1)) ++ ". " ++ (strin p) ++ "\n"
        printEm sub (n+1) r

strin :: Maybe T.Text -> String
strin Nothing = ""
strin (Just x) = (T.unpack x)

printComments :: Int -> Response ByteString -> String -> IO()
printComments n r s
    | s == "" = askForSub
    | otherwise = do
        putStrLn $ id (show (n+1)) ++ ". " ++ s ++ "\n"
        parseComments (n+1) r