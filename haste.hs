{-# LANGUAGE OverloadedStrings #-}
module Main (main) where 

import Network.HTTP.Conduit
import Network.HTTP.Base (urlEncodeVars)

import Data.Char (toLower)

import Data.Conduit
import Data.Conduit.Binary (sinkFile)

import System.Environment
import System.IO
import System.Directory
import System.FilePath

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Control.Monad.IO.Class (liftIO)


fields k s h f =  [ ("api_dev_key", k)
                  , ("api_option", "paste")
                  , ("api_paste_code", s)
                  , ("api_paste_private", "1")
                  , ("api_paste_format", h)
                  , ("api_paste_name", f)
                  ]

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
         src <-  B.readFile $ file
         reqp <- liftIO $ parseUrl "http://pastebin.com/api/api_post.php"
         manager <- liftIO $ newManager def
         let req3 = reqp
                    { method = "POST"
                    , checkStatus = \_ _ -> Nothing
                    }
         let key = "228eacb0a1dc707cbc0c7e290c08e799"
         Response _ _ _ fin <- runResourceT $
                               httpLbs (urlEncodedBody (fields key src (hilight file) (strictString $ takeFileName file)) req3) manager
         liftIO $ C.putStrLn fin


strictString = B.concat . L.toChunks . C.pack

hilight e
  | ext == ".hs"    = "haskell"
  | ext == ".c"     = "c"
  | ext == ".cmake" = "cmake"
  | ext == ".cpp"   = "cpp"
  | ext == ".sh"    = "bash"
  | ext == ".css"   = "css"
  | ext == ".d"     = "d"
  | ext == ".go"    = "go"
  | ext == ".java"  = "java"
  | ext == ".js"    = "javascript"
  | ext == ".latex" = "latex"
  | ext == ".lisp"  = "lisp"
  | ext == ".lua"   = "lua"
  | ext == ".pl"    = "perl" -- might be prolog...
  | ext == ".php"   = "php"
  | ext == ".py"    = "python"
  | ext == ".ruby"  = "ruby"
  | ext == ".tcl"   = "tcl"
  | ext == ".vb"    = "vb"
  | ext == ".xml"   = "xml"
  | otherwise       = "text"
  where ext = map toLower $ takeExtension e