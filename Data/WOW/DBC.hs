{-# LANGUAGE TypeFamilies,TemplateHaskell #-}
module Data.WOW.DBC( fieldI, fieldS, new, records, offset
                   , Field(..)
                   , CreatureModelDB
                   , CreatureSkinDB
                   , ) where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Word

import {-# SOURCE #-} Data.WOW.World
import Data.WOW.FileSystem
import Data.WOW.Utils
 
fieldI :: (DBC a, f ~ Field a) => f -> Record a -> Int
fieldI f (Record d o) = let o' = fromIntegral $ o + offset f 
                        in  runGet getUInt $ BS.drop o' $ dbc_data d

fieldS :: (DBC a, f ~ Field a) => f -> Record a -> String
fieldS f (Record d o) = let o' = fromIntegral $ o + offset f
                            s  = runGet getUInt (BS.drop o' $ dbc_data d)
                        in  runGet getString $ BS.drop s $ dbc_strings d
 
class DBC a where
    data Field a
    new     :: ResourceId -> IO a
    records :: a -> [Record a]
    offset  :: Field a -> Word32

------------------------------------------------------------------------------------------

data Record a = Record{ rec_dbc    :: DbcDesc
                      , rec_offset :: !Word32
                      }

data DbcDesc = DbcDesc{ dbc_rnumber :: !Word32
                      , dbc_rsize   :: !Word32
                      , dbc_data    :: BS.ByteString
                      , dbc_strings :: BS.ByteString
                      }                      
 
newDBC :: ResourceId -> IO DbcDesc
newDBC rid = do
  Just bs <- findFile rid
  let [nr,nf,sr,ss] = runGet (sequence $ take 4 $ repeat getUInt) bs
      dat = BS.take (sr*nr) $ BS.drop 16 bs
      str = BS.take ss $ BS.drop (16 + sr*nr) bs
  return $ DbcDesc (fromIntegral nr) (fromIntegral sr) dat str

newtype CharHairGeosetsDB = CharHairGeosetsDB DbcDesc
instance DBC CharHairGeosetsDB where
    data Field CharHairGeosetsDB = CharHairGeosetID | HRace | HGender | HSection | HGeoset
    new a = newDBC a >>= return . CharHairGeosetsDB
    records (CharHairGeosetsDB a) = map (\i -> Record a (dbc_rsize a * i)) [1..dbc_rnumber a]
    offset a = 4 * case a of
                     CharHairGeosetID -> 0
                     HRace -> 1
                     HGender -> 2
                     HSection -> 3
                     HGeoset -> 4

newtype CharSectionsDB = CharSectionsDB DbcDesc
instance DBC CharSectionsDB where
    data Field CharSectionsDB = CharSectonID | CRace | CGender | CType | CTex1 | CTex2 | CTex3 | CSection | CColor | CSkinType | CFaceType | CFacialHairType | CHairType | CUnderwearType
    new a = newDBC a >>= return . CharSectionsDB
    records (CharSectionsDB a) = map (\i -> Record a (dbc_rsize a * i)) [1..dbc_rnumber a]
    offset a = 4 * case a of
                     CharSectonID -> 0
                     CRace -> 1
                     CGender -> 2
                     CType -> 3
                     CTex1 -> 4
                     CTex2 -> 5
                     CTex3 -> 6
                     CSection -> 8
                     CColor -> 9
                     CSkinType -> 0
                     CFaceType -> 1
                     CFacialHairType -> 2
                     CHairType -> 3
                     CUnderwearType -> 4

newtype CharRacesDB = CharRacesDB DbcDesc
instance DBC CharRacesDB where
    data Field CharRacesDB = CharRaceID | RShortName | RName | RGeoType1
    new a = newDBC a >>= return . CharRacesDB
    records (CharRacesDB a) = map (\i -> Record a (dbc_rsize a * i)) [1..dbc_rnumber a]
    offset a = 4 * case a of
                     CharRaceID -> 0
                     RShortName -> 6
                     RName -> 11
                     RGeoType1 -> 65

newtype CharFacialHairDB = CharFacialHairDB DbcDesc
instance DBC CharFacialHairDB where
    data Field CharFacialHairDB = FRace | FGender | FStyle | FGeoset100 | FGeoset300 | FGeoset200
    new a = newDBC a >>= return . CharFacialHairDB
    records (CharFacialHairDB a) = map (\i -> Record a (dbc_rsize a * i)) [1..dbc_rnumber a]
    offset a = 4 * case a of
                     FRace -> 0
                     FGender -> 1
                     FStyle -> 2
                     FGeoset100 -> 3
                     FGeoset300 -> 4
                     FGeoset200 -> 5

newtype CharClassesDB = CharClassesDB DbcDesc
instance DBC CharClassesDB where
    data Field CharClassesDB = CharClassID | CName
    new a = newDBC a >>= return . CharClassesDB
    records (CharClassesDB a) = map (\i -> Record a (dbc_rsize a * i)) [1..dbc_rnumber a]
    offset a = 4 * case a of
                     CharClassID -> 0
                     CName -> 4

newtype ItemDisplayDB = ItemDisplayDB DbcDesc
instance DBC ItemDisplayDB where
    data Field ItemDisplayDB = ItemDisplayID | IModel | IModel2 | ISkin | ISkin2 | IIcon | ITexture | IGloveGeosetFlags | IBracerGeosetFlags | IRobeGeosetFlags | IBootsGeosetFlags | IUnknown | IItemGroupSounds | IGeosetVisID1 | IGeosetVisID2 | ITexArmUpper | ITexArmLower | ITexHands | ITexChestUpper | ITexChestLower | ITexLegUpper | ITexLegLower | ITexFeet | IVisuals
    new a = newDBC a >>= return . ItemDisplayDB
    records (ItemDisplayDB a) = map (\i -> Record a (dbc_rsize a * i)) [1..dbc_rnumber a]
    offset a = 4 * case a of
                     ItemDisplayID -> 0
                     IModel -> 1
                     IModel2 -> 2
                     ISkin -> 3
                     ISkin2 -> 4
                     IIcon -> 5
                     ITexture -> 6
                     IGloveGeosetFlags -> 7
                     IBracerGeosetFlags -> 8
                     IRobeGeosetFlags -> 9
                     IBootsGeosetFlags -> 10
                     IUnknown -> 11
                     IItemGroupSounds -> 12
                     IGeosetVisID1 -> 13
                     IGeosetVisID2 -> 14
                     ITexArmUpper -> 15
                     ITexArmLower -> 16
                     ITexHands -> 17
                     ITexChestUpper -> 18
                     ITexChestLower -> 19
                     ITexLegUpper -> 20
                     ITexLegLower -> 21
                     ITexFeet -> 22
                     IVisuals -> 23

newtype ItemVisualDB = ItemVisualDB DbcDesc
instance DBC ItemVisualDB where
    data Field ItemVisualDB = ItemVisualID | VEffect1
    new a = newDBC a >>= return . ItemVisualDB
    records (ItemVisualDB a) = map (\i -> Record a (dbc_rsize a * i)) [1..dbc_rnumber a]
    offset a = 4 * case a of
                     ItemVisualID -> 0
                     VEffect1 -> 1

newtype ItemVisualEffectDB = ItemVisualEffectDB DbcDesc
instance DBC ItemVisualEffectDB where
    data Field ItemVisualEffectDB = ItemVisualEffectID | VModel
    new a = newDBC a >>= return . ItemVisualEffectDB
    records (ItemVisualEffectDB a) = map (\i -> Record a (dbc_rsize a * i)) [1..dbc_rnumber a]
    offset a = 4 * case a of
                     ItemVisualEffectID -> 0
                     VModel -> 1

newtype ItemSetDB = ItemSetDB DbcDesc
instance DBC ItemSetDB where
    data Field ItemSetDB = ItemSetID | IName | IItemIDBase
    new a = newDBC a >>= return . ItemSetDB
    records (ItemSetDB a) = map (\i -> Record a (dbc_rsize a * i)) [1..dbc_rnumber a]
    offset a = 4 * case a of
                     ItemSetID -> 0
                     IName -> 1
                     IItemIDBase -> 18

newtype StartOutfitDB = StartOutfitDB DbcDesc
instance DBC StartOutfitDB where
    data Field StartOutfitDB = StartOutfitID | SRace | SClass | SGender | SItemIDBase
    new a = newDBC a >>= return . StartOutfitDB
    records (StartOutfitDB a) = map (\i -> Record a (dbc_rsize a * i)) [1..dbc_rnumber a]
    offset a = 4 * case a of
                     StartOutfitID -> 0
                     SRace -> 4
                     SClass -> 5
                     SGender -> 6
                     SItemIDBase -> 2

newtype CreatureModelDB = CreatureModelDB DbcDesc
instance DBC CreatureModelDB where
    data Field CreatureModelDB = CreatureModelID | CreatureModelType | CreatureModelFilename
    new a = newDBC a >>= return . CreatureModelDB
    records (CreatureModelDB a) = map (\i -> Record a (dbc_rsize a * i)) [1..dbc_rnumber a]
    offset a = 4 * case a of
                     CreatureModelID -> 0
                     CreatureModelType -> 1
                     CreatureModelFilename -> 2

newtype CreatureSkinDB = CreatureSkinDB DbcDesc
instance DBC CreatureSkinDB where
    data Field CreatureSkinDB = CreatureSkinID | CreatureSkinModelID | CreatureSkinNPCID | CreatureSkin
    new a = newDBC a >>= return . CreatureSkinDB
    records (CreatureSkinDB a) = map (\i -> Record a (dbc_rsize a * i)) [1..dbc_rnumber a]
    offset a = 4 * case a of
                     CreatureSkinID -> 0
                     CreatureSkinModelID -> 1
                     CreatureSkinNPCID -> 3
                     CreatureSkin -> 6

newtype CreatureTypeDB = CreatureTypeDB DbcDesc
instance DBC CreatureTypeDB where
    data Field CreatureTypeDB = CreatureTypeID | CreatureTypeName
    new a = newDBC a >>= return . CreatureTypeDB
    records (CreatureTypeDB a) = map (\i -> Record a (dbc_rsize a * i)) [1..dbc_rnumber a]
    offset a = 4 * case a of
                     CreatureTypeID -> 0
                     CreatureTypeName -> 1

newtype NPCDB = NPCDB DbcDesc
instance DBC NPCDB where
    data Field NPCDB = NPCID | NRaceID | NGender | NSkinColor | NFace | NHairStyle | NHairColor | NFacialHair | NHelmID | NShoulderID | NShirtID | NChestID | NBeltID | NPantsID | NBootsID | NBracersID | NGlovesID | NTabardID | NCapeID | NFilename
    new a = newDBC a >>= return . NPCDB
    records (NPCDB a) = map (\i -> Record a (dbc_rsize a * i)) [1..dbc_rnumber a]
    offset a = 4 * case a of
                     NPCID -> 0
                     NRaceID -> 1
                     NGender -> 2
                     NSkinColor -> 3
                     NFace -> 4
                     NHairStyle -> 5
                     NHairColor -> 6
                     NFacialHair -> 7
                     NHelmID -> 8
                     NShoulderID -> 9
                     NShirtID -> 10
                     NChestID -> 11
                     NBeltID -> 12
                     NPantsID -> 13
                     NBootsID -> 14
                     NBracersID -> 15
                     NGlovesID -> 16
                     NTabardID -> 17
                     NCapeID -> 18
                     NFilename -> 20