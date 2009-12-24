module FileSystem(findFile, extension, localFilePath) where

import Text.Regex.PCRE.Light.Char8
import qualified System.FilePath.Windows as W
import System.FilePath
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Data.Char
import Data.Monoid
import Control.Exception
import Resource

import Debug.Trace

-- Basic file system interfaces
-- uri is indentified by the prefix: MPQ, FILE, etc.

findFile :: ResourceId -> IO BS.ByteString
findFile fpath = fromJust $ match local fpath [] ~> BS.readFile
                        ||| match mpq   fpath [] ~> BS.readFile . joinPath . (prefix ++) . W.splitDirectories . lower
                        ||| Just (assert False undefined)
    where
      prefix = ["..", "tmp"]

extension :: ResourceId -> String
extension fpath = fromJust $ match local fpath [] ~> takeExtension
                         ||| match mpq   fpath [] ~> W.takeExtension
                         ||| Just (assert False undefined)

localFilePath :: ResourceId -> String
localFilePath fpath = fromJust $ match local fpath [] ~> id ||| Just (assert False undefined)

mpq    = compile "^MPQ:(?P<>[\\w\\d]+(\\\\[\\w\\d.]+)*)$" [no_auto_capture]
local  = compile "^FILE:(?P<>.+)$" [no_auto_capture]
lower  = map toLower

infix  6 ~>
infixl 4 |||
(~>) m f = fmap (f . head . tail) m
(|||) x y = getFirst $ (First x) `mappend` (First y)
