module Data.WOW.FileSystem(FileSystem, MockMPQ(..), findFile, checkExt, localFilePath) where

import Text.Regex.PCRE.Light.Char8
import qualified System.FilePath.Windows as W
import System.FilePath
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Data.Char
import Data.Monoid
import Control.Exception

class FileSystem fs where
    findFile :: fs -> String -> IO (Maybe BS.ByteString)

-- MockMPQ is a filesystem mimics MPQ packages by local files.
-- That is, finding a file on local disk with the some path w.r.t. some MPQ package.
data MockMPQ = MockMPQ [String]

instance FileSystem MockMPQ where
    findFile (MockMPQ prefix) fpath = fromJust $ match local fpath [] ~> myhandle . BS.readFile
                                             ||| match mpq   fpath [] ~> myhandle . BS.readFile . joinPath . (prefix ++) . W.splitDirectories . lower
                                             ||| Just (putStrLn ("unknown resource type[" ++ fpath ++"]") >> return Nothing)
        where myhandle x = fmap Just x `catch` (\ (_ :: SomeException) -> return Nothing)

checkExt :: FilePath -> String -> Bool
checkExt fpath ext = fromJust $ match local fpath [] ~> (== lower ext) . lower . takeExtension
                            ||| match mpq   fpath [] ~> (== lower ext) . lower . W.takeExtension
                            ||| Just False

localFilePath :: String -> FilePath
localFilePath fpath = fromJust $ match local fpath [] ~> id ||| Just (assert False undefined)

mpq, local :: Regex
mpq     = compile "^MPQ:(?P<>[\\w\\d]+(\\\\[\\w\\d.-]+)*)$" [no_auto_capture]
local   = compile "^FILE:(?P<>.+)$" [no_auto_capture]
lower :: [Char] -> [Char]
lower   = map toLower

infix  6 ~>
infixl 4 |||
(~>) :: Maybe [a] -> (a -> b) -> Maybe b
(~>) m f  = fmap (f . head . tail) m
(|||) :: Maybe a -> Maybe a -> Maybe a
(|||) x y = getFirst $ (First x) `mappend` (First y)
