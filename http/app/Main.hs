{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Req
import Control.Monad.IO.Class (liftIO)
import HaskellSay(haskellSay)
import GHC.Generics
import Data.Aeson

main :: IO ()
main = do
    runReq defaultHttpConfig $ do
        res <- req GET
                   (https "jsonplaceholder.typicode.com" /: "posts" /~ (1 :: Int))
                   NoReqBody
                   jsonResponse
                   mempty
        let post = (responseBody res :: Post)
        liftIO $ haskellSay $ getPostText post
        where
            getPostText :: Post -> String
            getPostText post = title post <> ": " <> body post


data Post = Post
  { userId :: Int,
    id :: Int,
    title :: String,
    body :: String
  } deriving (Show, Generic)

instance FromJSON Post
