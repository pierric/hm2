{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
module Data.WOW.World where

import qualified Control.Monad.State.Lazy as S
import Data.Label
import qualified Data.Label.Monadic as LM
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import Control.Category((>>>))
import Control.Exception

import Data.WOW.DBC
import Data.WOW.FileSystem as FS
import Data.WOW.WorldTemplate

type ResourceId = String
data {--FileSystem f =>--} ResourceLoader f r 
    = ResourceLoader { _validate   :: ResourceId -> Bool
                     , _new        :: ResourceId -> World f r r }
    
data {--FileSystem f =>--} ResourceLibrary f r
    = ResourceLibrary{ _loader     :: [ResourceLoader f r]
                     , _collection :: M.Map ResourceId r }
    
data {--FileSystem f =>--} WorldState f r
    = WorldState { _filesystem       :: f
                 , _resLibrary       :: ResourceLibrary f r
                 , _db_creaturemodel :: Maybe CreatureModelDB 
                 , _db_creatureskin  :: Maybe CreatureSkinDB }

type World f r = S.StateT (WorldState f r) IO

$(mkLabels [''WorldState,''ResourceLibrary])

-- Resource
findResource :: FileSystem f => ResourceId -> World f r (Maybe r)
findResource id = do lib <- LM.gets (resLibrary >>> collection)
                     case M.lookup id lib of
                       Just x  -> return (Just x)
                       Nothing -> do chk <- LM.gets (resLibrary >>> loader) 
                                     -- get those valid loaders
                                     case filter (\ldr -> _validate ldr id) chk of
                                       ldr:_ -> do res <- _new ldr id
                                                   LM.modify (resLibrary >>> collection) (M.insert id res)
                                                   return (Just res)
                                       []    -> return Nothing

--findFile :: FileSystem f => ResourceId -> World f r (Maybe BS.ByteString)
--findFile id = do fs <- LM.gets filesystem
--                 lift $ FS.findFile fs id
                 

-- World
$(db [("CreatureSkinDB"  ,"MPQ:DBFilesClient\\CreatureDisplayInfo.dbc")
     ,("CreatureModelDB" ,"MPQ:DBFilesClient\\CreatureModelData.dbc"  )])
