{-# LANGUAGE TypeOperators  #-}
module Data.WOW.World where

import Data.Record.Label
import Control.Monad.State
import qualified Data.Map as M

-- Resource
type ResourceId = String
data ResourceLoader res  = ResourceLoader { _validate   :: ResourceId -> Bool
                                          , _new        :: ResourceId -> World res res }
data ResourceLibrary res = ResourceLibrary{ _loader     :: [ResourceLoader res]
                                          , _collection :: M.Map ResourceId res }

findResource :: ResourceId -> World a (Maybe a)

-- World
data WorldState res
type World res = StateT (WorldState res) IO

resLibrary ::  WorldState res :-> ResourceLibrary res