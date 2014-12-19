{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, TypeOperators #-}
module Data.WOW.World where

import Data.Label
import qualified Data.Map as M
import Control.Category((>>>))
import Language.Haskell.TH
import Data.Char(toLower)
import Control.Monad.Trans
import Prelude hiding (mod)

import Data.WOW.DBC
import Data.WOW.FileSystem as FS

type ResourceId = String
data ResourceLoader m r 
   = ResourceLoader { _validate   :: ResourceId -> Bool
                    , _new        :: ResourceId -> m r }
    
data ResourceLibrary m r
   = ResourceLibrary{ _loader     :: [ResourceLoader m r]
                    , _collection :: M.Map ResourceId r }
    
data WorldState m f r
   = WorldState { _filesystem       :: f
                , _resLibrary       :: ResourceLibrary m r
                , _db_creaturemodel :: Maybe CreatureModelDB 
                , _db_creatureskin  :: Maybe CreatureSkinDB }

class (MonadIO m, FS.FileSystem f) => M2World m f r | m -> f r where
    getWorld :: m (WorldState m f r)
    modWorld :: (WorldState m f r -> WorldState m f r) -> m ()

getWorldField :: (Monad m, M2World m f r) => (WorldState m f r :-> b) -> m b
getWorldField proj = getWorld >>= (return . get proj)
setWorldField :: (Monad m, M2World m f r) => (WorldState m f r :-> a) -> a -> m ()
setWorldField proj val = modWorld (set proj val)
modWorldField :: (Monad m, M2World m f r) => (WorldState m f r :-> a) -> (a -> a) -> m ()
modWorldField proj mod = modWorld (modify proj mod)

-- $(mkLabels [''WorldState,''ResourceLibrary])
filesystem :: WorldState m f r :-> f
filesystem = lens _filesystem (\mod w -> w { _filesystem = mod (_filesystem w) })

resLibrary :: WorldState t f r :-> ResourceLibrary t r
resLibrary = lens _resLibrary (\mod w -> w { _resLibrary = mod (_resLibrary w) })

db_creaturemodel :: WorldState m f r :-> Maybe CreatureModelDB
db_creaturemodel = lens _db_creaturemodel (\mod w -> w { _db_creaturemodel = mod (_db_creaturemodel w) })

db_creatureskin  :: WorldState m f r :-> Maybe CreatureSkinDB
db_creatureskin  = lens _db_creatureskin  (\mod w -> w { _db_creatureskin  = mod (_db_creatureskin  w) })

collection :: ResourceLibrary m r :-> M.Map ResourceId r
collection = lens _collection (\mod l -> l { _collection = mod (_collection l) })

loader     :: ResourceLibrary t r :-> [ResourceLoader t r]
loader     = lens _loader     (\mod l -> l { _loader     = mod (_loader     l) })


-- Resource
findResource :: M2World m f r => ResourceId -> m (Maybe r)
findResource resid = do 
    lib <- getWorldField (resLibrary >>> collection)
    case M.lookup resid lib of
      Just x  -> return (Just x)
      Nothing -> do chk <- getWorldField (resLibrary >>> loader) 
                    -- get those valid loaders
                    case filter (flip _validate resid) chk of
                      ldr:_ -> do res <- _new ldr resid
                                  modWorldField (resLibrary >>> collection) (M.insert resid res)
                                  return (Just res)
                      []    -> return Nothing

mkDB :: [Char] -> String -> Q [Dec]
mkDB name source = do x <- let vm = mkName "m"
                               vf = mkName "f"
                               vr = mkName "r"
                           in  sigD funcName (forallT [PlainTV vm, PlainTV vf, PlainTV vr]
                                                  (cxt [classP (mkName "M2World") [varT vm, varT vf, varT vr]])
                                                  (varT vm `appT`  (conT $ mkName name)))
                      y <- funD funcName [clause [] (normalB body) []]
                      return [x,y]
    where body = [| do m <- getWorldField $(varE dbName)
                       case m of
                         Just a  -> return a
                         Nothing -> do fs <- getWorldField $(varE (mkName "filesystem"))
                                       ct <- liftIO (findFile fs $(litE $ stringL source))
                                       case ct of 
                                         Nothing -> error $ "Cannot find DBC:" ++ $(litE $ stringL source)
                                         Just db -> let db' = open db
                                                    in  setWorldField $(varE dbName) (Just db')  >> return db' |]
          funcName = mkName $ toLower (head name) : tail name
          dbName   = mkName $ "db_" ++ let l = length name in take (l-2) (map toLower name)

mkDBs :: [([Char], String)] -> Q [Dec]
mkDBs = fmap concat . mapM (uncurry mkDB)

