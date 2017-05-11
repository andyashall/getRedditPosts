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
    -- Filters for lens (the json keys)
    let f1 = "data" :: T.Text
    let f2 = "children" :: T.Text
    let f3 = "title" :: T.Text
    -- This gets the posts array from the json
    let getPosts = responseBody . key f1 . key f2
    let dat = r ^? responseBody . key f1 . key f2
    -- let eachN = dat & each %~ T.toUpper
    let t1 = r ^? getPosts . nth 0 . key f1 . key f3 . _String
    let t2 = r ^? getPosts . nth 1 . key f1 . key f3 . _String
    let t3 = r ^? getPosts . nth 2 . key f1 . key f3 . _String
    let t4 = r ^? getPosts . nth 3 . key f1 . key f3 . _String
    let t5 = r ^? getPosts . nth 4 . key f1 . key f3 . _String
    let t6 = r ^? getPosts . nth 5 . key f1 . key f3 . _String
    let t7 = r ^? getPosts . nth 6 . key f1 . key f3 . _String
    let t8 = r ^? getPosts . nth 7 . key f1 . key f3 . _String
    let t9 = r ^? getPosts . nth 8 . key f1 . key f3 . _String
    let t10 = r ^? getPosts . nth 9 . key f1 . key f3 . _String
    print $ "1. " Prelude.++ (strin t1)
    print $ "2. " Prelude.++ (strin t2)
    print $ "3. " Prelude.++ (strin t3)
    print $ "4. " Prelude.++ (strin t4)
    print $ "5. " Prelude.++ (strin t5)
    print $ "6. " Prelude.++ (strin t6)
    print $ "7. " Prelude.++ (strin t7)
    print $ "8. " Prelude.++ (strin t8)
    print $ "9. " Prelude.++ (strin t9)
    print $ "10. " Prelude.++ (strin t10)

strin :: Maybe T.Text -> String
strin Nothing = ""
strin (Just x) = (T.unpack x)