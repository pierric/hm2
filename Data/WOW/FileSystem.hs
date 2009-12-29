module Data.WOW.FileSystem(findFile, checkExt, localFilePath) where

import Text.Regex.PCRE.Light.Char8
import qualified System.FilePath.Windows as W
import System.FilePath
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Data.Char
import Data.Monoid
import Control.Exception
import Debug.Trace

import {-# SOURCE #-} Data.WOW.World


-- Basic file system interfaces
-- uri is indentified by the prefix: MPQ, FILE, etc.

findFile :: ResourceId -> IO (Maybe BS.ByteString)
findFile fpath = fromJust $ match local fpath [] ~> myhandle . BS.readFile
                        ||| match mpq   fpath [] ~> myhandle . BS.readFile
                                                    . joinPath . (prefix ++) . W.splitDirectories . lower
                        ||| Just (putStrLn ("unknown resource type[" ++ fpath ++"]") >> return Nothing)
    where
      prefix = ["..", "tmp"]
      myhandle x = fmap Just x `Prelude.catch` (\_ -> return Nothing)

checkExt :: FilePath -> String -> Bool
checkExt fpath ext = fromJust $ match local fpath [] ~> (== lower ext) . lower . takeExtension
                            ||| match mpq   fpath [] ~> (== lower ext) . lower . W.takeExtension
                            ||| Just False

localFilePath :: ResourceId -> String
localFilePath fpath = fromJust $ match local fpath [] ~> id ||| Just (assert False undefined)

mpq     = compile "^MPQ:(?P<>[\\w\\d]+(\\\\[\\w\\d.-]+)*)$" [no_auto_capture]
local   = compile "^FILE:(?P<>.+)$" [no_auto_capture]
lower   = map toLower

infix  6 ~>
infixl 4 |||
(~>) m f = fmap (f . head . tail) m
(|||) x y = getFirst $ (First x) `mappend` (First y)
