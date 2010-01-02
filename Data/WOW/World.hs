{-# LANGUAGE TemplateHaskell #-}
module Data.WOW.World where

import Control.Monad.State
import Data.Record.Label
import qualified Data.Map as M
import Control.Category((>>>))
import Control.Exception

import Data.WOW.DBC
import Data.WOW.WorldTemplate

type ResourceId = String
data ResourceLoader r  = ResourceLoader { _validate   :: ResourceId -> Bool
                                        , _new        :: ResourceId -> World r r }

data ResourceLibrary r = ResourceLibrary{ _loader     :: [ResourceLoader r]
                                        , _collection :: M.Map ResourceId r }

data WorldState r      = WorldState { _resLibrary       :: ResourceLibrary r
                                    , _db_creaturemodel :: Maybe CreatureModelDB 
                                    , _db_creatureskin  :: Maybe CreatureSkinDB  
                                    }
type World r = StateT (WorldState r) IO

$(mkLabels [''WorldState,''ResourceLibrary])

-- Resource
findResource :: ResourceId -> World r (Maybe r)
findResource id = do lib <- getM (resLibrary >>> collection)
                     case M.lookup id lib of
                       Just x  -> return (Just x)
                       Nothing -> do chk <- getM (resLibrary >>> loader) 
                                     -- get those valid loaders
                                     case filter (\ldr -> _validate ldr id) chk of
                                       ldr:_ -> do res <- _new ldr id
                                                   modM (resLibrary >>> collection) (M.insert id res)
                                                   return (Just res)
                                       []    -> return Nothing

-- World
$(db [("CreatureSkinDB"  ,"MPQ:DBFilesClient\\CreatureDisplayInfo.dbc")
     ,("CreatureModelDB" ,"MPQ:DBFilesClient\\CreatureModelData.dbc"  )])