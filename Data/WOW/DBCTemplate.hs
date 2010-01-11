{-# LANGUAGE TemplateHaskell #-}
module Data.WOW.DBCTemplate where

import Text.PrettyPrint.HughesPJ
import Data.List

declDBC name field = vcat ([ hsep $ map text ["newtype", name, "=", name, "DbcDesc"]
                           , hsep $ map text ["instance DBC", name, "where"]
                           , nest 4 $ hsep $ map text $ ["data Field", name, "="] ++ intersperse "|" (map fst field)
                           , nest 4 $ hsep $ map text $ ["open =", name, ". openDBCfromByteString"]
                           , nest 4 $ hcat $ map text $ ["records (", name, " a) = map (\\i -> Record a (dbc_rsize a * i)) [0..dbc_rnumber a-1]"]
                           , nest 4 $ text "offset a = 4 * case a of" ]
                           ++
                           map (\(n,v) -> nest 21 $ text n <+> text "->" <+> int v) field)
                 
d = [ declDBC "CharHairGeosetsDB" [("CharHairGeosetID", 0)
                                  ,("HRace", 1)
                                  ,("HGender", 2)
                                  ,("HSection",3)
                                  ,("HGeoset",4)]
    , declDBC "CharSectionsDB" [("CharSectonID", 0)
	                       ,("CRace", 1)
	                       ,("CGender", 2)
	                       ,("CType", 3)
	                       ,("CTex1", 4)
	                       ,("CTex2", 5)
	                       ,("CTex3", 6)
	                       ,("CSection", 8)
	                       ,("CColor", 9)
	                       ,("CSkinType", 0)
	                       ,("CFaceType", 1)
	                       ,("CFacialHairType", 2)
	                       ,("CHairType", 3)
	                       ,("CUnderwearType", 4)]
    , declDBC "CharRacesDB" [("CharRaceID", 0)
	                    ,("RShortName", 6)
	                    ,("RName", 11)
                            ,("RGeoType1", 65) ]
    , declDBC "CharFacialHairDB" [("FRace", 0)
	                         ,("FGender", 1)
	                         ,("FStyle", 2)
	                         ,("FGeoset100", 3)
	                         ,("FGeoset300", 4)
	                         ,("FGeoset200", 5) ]
    , declDBC "CharClassesDB" [("CharClassID", 0)
	                      ,("CName", 4) ]
    , declDBC "ItemDisplayDB" [("ItemDisplayID", 0)
	                      ,("IModel", 1)
	                      ,("IModel2", 2)
	                      ,("ISkin", 3)
	                      ,("ISkin2", 4)
	                      ,("IIcon", 5)
	                      ,("ITexture", 6)
	                      ,("IGloveGeosetFlags", 7)
	                      ,("IBracerGeosetFlags", 8)
	                      ,("IRobeGeosetFlags", 9)
	                      ,("IBootsGeosetFlags", 10)
	                      ,("IUnknown", 11)
	                      ,("IItemGroupSounds", 12)
	                      ,("IGeosetVisID1", 13)
	                      ,("IGeosetVisID2", 14)
	                      ,("ITexArmUpper", 15)
	                      ,("ITexArmLower", 16)
	                      ,("ITexHands", 17)
	                      ,("ITexChestUpper", 18)
	                      ,("ITexChestLower", 19)
	                      ,("ITexLegUpper", 20)
	                      ,("ITexLegLower", 21)
	                      ,("ITexFeet", 22)
	                      ,("IVisuals", 23) ]
    , declDBC "ItemVisualDB" [("ItemVisualID", 0)
	                     ,("VEffect1", 1)]
    , declDBC "ItemVisualEffectDB" [("ItemVisualEffectID", 0)
	                           ,("VModel", 1)]
    , declDBC "ItemSetDB" [("ItemSetID", 0)
	                  ,("IName", 1)
	                  ,("IItemIDBase", 18)]
    , declDBC "StartOutfitDB" [("StartOutfitID", 0)
	                      ,("SRace", 4)
	                      ,("SClass", 5)
	                      ,("SGender", 6)
	                      ,("SItemIDBase", 2)]
    , declDBC "CreatureModelDB" [("CreatureModelID", 0)
	                        ,("CreatureModelType", 1)
	                        ,("CreatureModelFilename", 2) ]
    , declDBC "CreatureSkinDB" [("CreatureSkinID", 0)
	                       ,("CreatureSkinModelID", 1)
	                       ,("CreatureSkinNPCID", 3)
	                       ,("CreatureSkin0", 6) 
                               ,("CreatureSkin1", 7) 
                               ,("CreatureSkin2", 8) ]
    , declDBC "CreatureTypeDB" [("CreatureTypeID", 0)
	                       ,("CreatureTypeName", 1)]
    , declDBC "NPCDB" [("NPCID", 0)
	              ,("NRaceID", 1)
	              ,("NGender", 2)
	              ,("NSkinColor", 3)
	              ,("NFace", 4)
	              ,("NHairStyle", 5)
	              ,("NHairColor", 6)
	              ,("NFacialHair", 7)
	              ,("NHelmID", 8)
	              ,("NShoulderID", 9)
	              ,("NShirtID", 10)
	              ,("NChestID", 11)
	              ,("NBeltID", 12)
	              ,("NPantsID", 13)
	              ,("NBootsID", 14)
	              ,("NBracersID", 15)
	              ,("NGlovesID", 16)
	              ,("NTabardID", 17)
	              ,("NCapeID", 18)
	              ,("NFilename", 20) ]
    ]

gen = writeFile "DBC.hs" $ render $ vcat $ map text header ++ [text ""] ++ intersperse (text "") d

    where 
      header = ["{-# LANGUAGE TypeFamilies #-}"
               ,"module Data.WOW.DBC( fieldI, fieldS, open, records, offset"
               ,"                   , Field(..)"
               ,"                   , CreatureModelDB"
               ,"                   , CreatureSkinDB"
               ,"                   , ) where"
               ,""
               ,"import qualified Data.ByteString.Lazy as BS"
               ,"import Data.Binary.Get"
               ,"import Data.Word"
               ,""
               ,"import FileSystem"
               ,"import Utils"
               ," "
               ,"fieldI :: (DBC a, f ~ Field a) => f -> Record a -> Int"
               ,"fieldI f (Record d o) = let o' = fromIntegral $ o + offset f "
               ,"                        in  runGet getUInt $ BS.drop o' $ dbc_data d"
               ,""
               ,"fieldS :: (DBC a, f ~ Field a) => f -> Record a -> String"
               ,"fieldS f (Record d o) = let o' = fromIntegral $ o + offset f"
               ,"                            s  = runGet getUInt (BS.drop o' $ dbc_data d)"
               ,"                        in  runGet getString $ BS.drop s $ dbc_strings d"
               ," "
               ,"class DBC a where"
               ,"    data Field a"
               ,"    open    :: BS.ByteString -> a"
               ,"    records :: a -> [Record a]"
               ,"    offset  :: Field a -> Word32"
               ,""
               ,"------------------------------------------------------------------------------------------"
               ,""
               ,"data Record a = Record{ rec_dbc    :: DbcDesc"
               ,"                      , rec_offset :: !Word32"
               ,"                      }"
               ,""
               ,"data DbcDesc = DbcDesc{ dbc_rnumber :: !Word32"
               ,"                      , dbc_rsize   :: !Word32"
               ,"                      , dbc_data    :: BS.ByteString"
               ,"                      , dbc_strings :: BS.ByteString"
               ,"                      }                      "
               ," "
               ,"openDBC :: FilePath -> IO DbcDesc"
               ,"openDBC rid = BS.readFile rid  >>= return . openDBCfromByteString"
               ,""
               ,"openDBCfromByteString :: BS.ByteString -> DbcDesc"
               ,"openDBCfromByteString bs = "
               ,"  let [hd,nr,nf,sr,ss] = runGet (sequence $ take 5 $ repeat getUInt) bs"
               ,"      !dat = BS.take (sr*nr) $ BS.drop 20 bs"
               ,"      !str = BS.take ss $ BS.drop (20 + sr*nr) bs"
               ,"  in  assert (hd == 1128416343) $ DbcDesc (fromIntegral nr) (fromIntegral sr) dat str"
               ]
