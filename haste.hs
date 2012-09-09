{-# LANGUAGE OverloadedStrings #-}
module Main (main) where 

import Network.HTTP.Conduit
import Network.HTTP.Base (urlEncodeVars)

import Data.Conduit
import Data.Conduit.Binary (sinkFile)

import System.Environment
import System.IO
import System.Directory
import System.FilePath

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Control.Monad.IO.Class (liftIO)


fields k s =  [("api_dev_key", k), ("api_option", "paste"), ("api_paste_code", s), ("api_paste_private", "1")] 
mlist :: [a] -> Int -> Maybe a
l `mlist` i 
    | (length l) - 1 < i = Nothing
    | otherwise        = Just $ l !! i 

main :: IO ()
main = do
  args <- getArgs
  case args `mlist` 0 of
    Nothing -> putStrLn "Need a file to paste."
    Just file -> do
         home <- getHomeDirectory
         --key <- B.readFile $ home </> ".haste"
         src <-  B.readFile $ file
         reqp <- liftIO $ parseUrl "http://pastebin.com/api/api_post.php"
         manager <- liftIO $ newManager def
         let req3 = reqp
                    { method = "POST"
                    , checkStatus = \_ _ -> Nothing
                    }
         let key = "228eacb0a1dc707cbc0c7e290c08e799"
         Response _ _ _ fin <- runResourceT $ httpLbs (urlEncodedBody (fields key src) req3) manager
         liftIO $ L.putStr fin