{-# LANGUAGE TemplateHaskell #-}
module Data.WOW.WorldTemplate where

import Data.WOW.World
import Data.WOW.DBC

-- World
$(mkDBs [("CreatureSkinDB"  ,"MPQ:DBFilesClient\\CreatureDisplayInfo.dbc")
        ,("CreatureModelDB" ,"MPQ:DBFilesClient\\CreatureModelData.dbc"  )])
