module Data.WOW.Creature where

import System.FilePath.Windows
import Debug.Trace
import Control.Exception
import Data.List(nub)
import Data.Maybe(catMaybes)

import Data.WOW.World
import Data.WOW.DBC
import Data.WOW.Utils

type Skin = [Maybe ResourceId]

data Creature = Creature{ _crea_name     :: String
                        , _crea_resource :: ResourceId
                        , _crea_skin     :: Skin
                        -- more information to characterize a creature,
                        -- such as its position, status, behavior, etc.
                        -- _crea_position :: Vector3 Float 
                        }

-- newCreature :: FileSystem f => String -> Int -> World f r Creature
newCreature fn skin = do sk <- creatureSkinList fn
                         let ss = case lookup skin (zip [0..] sk) of
                                    Just x  -> x                                 -- the chosen skin
                                    Nothing -> if null sk                        -- if no available skin
                                               then repeat Nothing               -- then choose a empty one
                                               else head sk                      -- else take the first one
                         return $ Creature{ _crea_name = fn
                                          , _crea_resource = ("MPQ:" ++ fn ++ ".m2")
                                          , _crea_skin     = ss }

-- creatureSkinList :: FileSystem f => String -> World f r [Skin]
creatureSkinList fn = assert (not (null fn)) $
                      do mdb <- creatureModelDB
                         sdb <- creatureSkinDB
                         let rec   = filter ((cmpString (fn ++ ".mdx")) . fieldS CreatureModelFilename) $ records mdb 
                         return $ case rec of 
                                    []  -> []
                                    [x] -> if fieldI CreatureModelType x /= 4
                                           then let id = fieldI CreatureModelID x
                                                    rs = filter ((==id) . fieldI CreatureSkinModelID) $ records sdb
                                                    ss = nub $ map (\r -> replicate 11 Nothing ++
                                                                          [maybe $ fieldS CreatureSkin0 r
                                                                          ,maybe $ fieldS CreatureSkin1 r
                                                                          ,maybe $ fieldS CreatureSkin2 r]) rs
                                                in  filter (not . null . catMaybes) ss
                                           else []
    where maybe "" = Nothing
          maybe x  = let r = tail $ reverse $ splitPath fn
                     in  assert (not (null r)) $ Just $ "MPQ:" ++ (joinPath $ reverse (x:r)) ++ ".blp"

creatureChangeSkin creature idx = newCreature (_crea_name creature) idx