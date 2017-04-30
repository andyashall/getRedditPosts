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

-- data Post = Post {
-- 	hello :: T.Text
--   , fella :: Object
-- } deriving (Show,G.Generic)

-- data Posts = Posts {
-- 	after :: T.Text
--   , before :: T.Text
--   , children :: Post
--   , modhash :: T.Text
-- } deriving (Show,G.Generic)

-- data Data = Data {
-- 	mate :: T.Text
--   , fell :: Posts
-- } deriving (Show,G.Generic)

-- instance FromJSON Data
-- instance ToJSON Data

-- instance FromJSON Posts
-- instance ToJSON Posts

-- instance FromJSON Post
-- instance ToJSON Post

askForSub :: IO ()
askForSub = do
	Prelude.putStrLn "Enter a Subreddit"
	subreddit <- getLine
	posts <- getPosts subreddit
	print "done"

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
        -- let combined = t1
        return $ t1

strin :: Maybe T.Text -> String
strin Nothing = ""
strin (Just x) = (T.unpack x)