{-# LANGUAGE TemplateHaskell #-}
module Data.WOW.World where

import Control.Monad.State
import Data.Record.Label
import qualified Data.Map as M
import Control.Category((>>>))
import Control.Exception

import Data.WOW.DBC
import Data.WOW.WorldTemplate
import Data.WOW.GL.ResourceLoader


type ResourceId = String
data ResourceLoader res  = ResourceLoader { _validate   :: ResourceId -> Bool
                                          , _new        :: ResourceId -> World res res }

data ResourceLibrary res = ResourceLibrary{ _loader     :: [ResourceLoader res]
                                          , _collection :: M.Map ResourceId res }

data WorldState res      = WorldState { _resLibrary :: ResourceLibrary res
                                      , _db_creaturemodel :: Maybe CreatureModelDB 
                                      , _db_creatureskin  :: Maybe CreatureSkinDB  
                                      }
type World res = StateT (WorldState res) IO

$(mkLabels [''WorldState,''ResourceLibrary])

-- Resource
loadResource :: ResourceId -> World a a
loadResource fpath = do res <- findResource fpath
                        case res of 
                          -- resource is already loaded
                          Just x  -> return x
                          -- load the resource and add it to library
                          Nothing -> do chk <- getM (resLibrary >>> loader) 
                                        -- get those valid loaders
                                        case filter (\ldr -> _validate ldr fpath) chk of
                                          ldr:_ -> do res <- _new ldr fpath
                                                      modM (resLibrary >>> collection) (M.insert fpath res)
                                                      return res
                                          []    -> assert False undefined

findResource :: ResourceId -> World a (Maybe a)
findResource id = do lib <- getM (resLibrary >>> collection)
                     return $ M.lookup id lib
-- World
$(db [("CreatureSkinDB"  ,"DBFilesClient\\CreatureDisplayInfo.dbc")
     ,("CreatureModelDB" ,"DBFilesClient\\CreatureModelData.dbc"  )])

beginning = WorldState{ _resLibrary = (ResourceLibrary glResourceLoader M.empty) }