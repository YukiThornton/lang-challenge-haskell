{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Req
import Control.Monad.IO.Class (liftIO)
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import Prelude hiding (id)
import qualified Data.Text.IO as DTI (putStrLn)

main :: IO ()
main = do
    runReq defaultHttpConfig $ do
        res <- req GET
                   (https "jsonplaceholder.typicode.com" /: "posts")
                   NoReqBody
                   jsonResponse
                   mempty
        let post = (responseBody res :: [Post])
        liftIO $ DTI.putStrLn $ T.intercalate "\n" $ map getPostText $ take 5 post
        where
            getPostText :: Post -> T.Text
            getPostText post = "Id(" <> T.pack (show (id post)) <> "), Title(" <> title post <> "), Body(" <> T.take 20 (body post) <> "...)"


data Post = Post
  { userId :: Int,
    id :: Int,
    title :: T.Text,
    body :: T.Text
  } deriving (Show, Generic)

instance FromJSON Post
