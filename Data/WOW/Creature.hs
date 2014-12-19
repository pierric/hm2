module Data.WOW.Creature where

import System.FilePath.Windows
import Control.Exception
import Data.List(nub)
import Data.Maybe(catMaybes)

import Data.WOW.World
import Data.WOW.WorldTemplate
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

newCreature :: (Monad m, M2World m f r) => FilePath -> Int -> m Creature
newCreature fn skin = do sk <- creatureSkinList fn
                         let ss = case lookup skin (zip [0..] sk) of
                                    Just x  -> x                                 -- the chosen skin
                                    Nothing -> if null sk                        -- if no available skin
                                               then repeat Nothing               -- then choose a empty one
                                               else head sk                      -- else take the first one
                         return $ Creature{ _crea_name = fn
                                          , _crea_resource = ("MPQ:" ++ fn ++ ".m2")
                                          , _crea_skin     = ss }

creatureSkinList :: (Monad m, M2World m f r) => FilePath -> m [Skin]
creatureSkinList fn = assert (not (null fn)) $
                      do mdb <- creatureModelDB
                         sdb <- creatureSkinDB
                         let rec   = filter ((cmpString (fn ++ ".mdx")) . fieldS CreatureModelFilename) $ records mdb 
                         return $ nub $ concatMap (\x -> if fieldI CreatureModelType x /= 4
                                                         then let id_ = fieldI CreatureModelID x
                                                                  rs  = filter ((==id_) . fieldI CreatureSkinModelID) $ records sdb
                                                                  ss  = nub $ map (\r -> replicate 11 Nothing ++
                                                                                         [maybe_ $ fieldS CreatureSkin0 r
                                                                                         ,maybe_ $ fieldS CreatureSkin1 r
                                                                                         ,maybe_ $ fieldS CreatureSkin2 r]) rs
                                                              in  filter (not . null . catMaybes) ss
                                                         else []) rec
    where maybe_ "" = Nothing
          maybe_ x  = let r = tail $ reverse $ splitPath fn
                     in  assert (not (null r)) $ Just $ "MPQ:" ++ (joinPath $ reverse (x:r)) ++ ".blp"

creatureChangeSkin :: (Monad m, M2World m f r) => Creature -> Int -> m Creature
creatureChangeSkin creature idx = newCreature (_crea_name creature) idx
