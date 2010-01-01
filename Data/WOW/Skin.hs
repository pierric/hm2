module Data.WOW.Skin where

import Data.WOW.World
import Data.WOW.M2Model
import Data.WOW.DBC
import Debug.Trace

creatureSkinCount :: M2Model -> World r Int
creatureSkinCount mdl = do mdb <- creatureModelDB
                           sdb <- creatureSkinDB
                           let rec = filter ((== fn) . fieldS CreatureModelFilename) $ records mdb 
                           case rec of 
                             []  -> return 0
                             [x] -> if fieldI CreatureModelType x /= 4
                                    then return 0
                                    else let id = fieldI CreatureModelID x
                                             rs = filter ((==id) . fieldI CreatureSkinModelID) $ records sdb
                                             skinset = map (\r -> (fieldS CreatureSkin0 r
                                                                  ,fieldS CreatureSkin1 r
                                                                  ,fieldS CreatureSkin2 r)) rs
                                         in  trace fn $ traceShow skinset $ return $ length rs
    where fn = m_name_ mdl

--creatureSkinFind  :: M2Model -> Int -> World r [Maybe ResourceId]