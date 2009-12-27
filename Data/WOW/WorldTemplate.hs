{-# LANGUAGE TemplateHaskell #-}
module Data.WOW.WorldTemplate where

import Data.Record.Label
import Language.Haskell.TH
import Data.Char(toLower)
import Control.Monad.Trans(lift)

mkDB name source = do x <- let va = mkName "a"
                           in  sigD funcName (forallT [va] (cxt []) $
                                                      (conT $ mkName "World") `appT`
                                                      (varT va) `appT`
                                                      (conT $ mkName name))
                      y <- funD funcName [clause [] (normalB body) []]
                      return [x,y]
    where body = [| getM $(varE dbName) >>= 
                    (\m -> case m of Just a  -> return a
                                     Nothing -> lift $(appE (varE (mkName "new")) (litE $ stringL source)) >>= 
                                                (\d -> setM $(varE dbName) (Just d) >> return d)) |]
          funcName = mkName $ toLower (head name) : tail name
          dbName   = mkName $ "db_" ++ let l = length name in take (l-2) (map toLower name)

db = fmap concat . mapM (uncurry mkDB)