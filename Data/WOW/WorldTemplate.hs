{-# LANGUAGE TemplateHaskell #-}
module Data.WOW.WorldTemplate(db) where

import Data.Record.Label
import Language.Haskell.TH
import Data.Char(toLower)
import Control.Monad.Trans(lift)

import Data.WOW.FileSystem
import Data.WOW.DBC

mkDB name source = do x <- let va = mkName "a"
                               vb = mkName "b"
                           in  sigD funcName (forallT [PlainTV va,PlainTV vb]
                                                  (cxt [classP (mkName "FileSystem") [varT va]])
                                                  ((conT $ mkName "World") 
                                                   `appT` (varT va) 
                                                   `appT` (varT vb)
                                                   `appT` (conT $ mkName name)))
                      y <- funD funcName [clause [] (normalB body) []]
                      return [x,y]
    where body = [| getM $(varE dbName) >>= 
                    (\m -> case m of Just a  -> return a
                                     Nothing -> do fs <- getM $(varE (mkName "filesystem"))
                                                   ct <- lift (findFile fs $(litE $ stringL source))
                                                   case ct of 
                                                     Nothing -> error $ "Cannot find DBC:" ++ $(litE $ stringL source)
                                                     Just db -> let db' = open db
                                                                in  setM $(varE dbName) (Just db')  >> return db') |]
          funcName = mkName $ toLower (head name) : tail name
          dbName   = mkName $ "db_" ++ let l = length name in take (l-2) (map toLower name)

db = fmap concat . mapM (uncurry mkDB)