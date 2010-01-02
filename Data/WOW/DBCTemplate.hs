{-# LANGUAGE TemplateHaskell #-}
module Data.WOW.DBCTemplate where

--import Data.Word
--import qualified Data.ByteString.Lazy as BS
--import Data.Binary.Get
--import Language.Haskell.TH
import Text.PrettyPrint.HughesPJ
import Data.List

declDBC name field = vcat ([ hsep $ map text ["newtype", name, "=", name, "DbcDesc"]
                           , hsep $ map text ["instance DBC", name, "where"]
                           , nest 4 $ hsep $ map text $ ["data Field", name, "="] ++ intersperse "|" (map fst field)
                           , nest 4 $ hsep $ map text $ ["new a = newDBC a >>= return .", name]
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

gen = do 
  writeFile "DBC.hs" $ render $ vcat $ map text header ++ [text ""] ++ intersperse (text "") d

    where 
      header = ["{-# LANGUAGE TypeFamilies,TemplateHaskell #-}"
               ,"module DBC( fieldI, fieldS, new, records, offset"
               ,"          , Field(..)"
               ,"          , CreatureModelDB"
               ,"          , CreatureSkinDB"
               ,"          , ) where"
               ,""
               ,"import qualified Data.ByteString.Lazy as BS"
               ,"import Data.Binary.Get"
               ,"import Data.Word"
               ,""
               ,"import {-#SOURCE#-}World"
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
               ,"    new     :: ResourceId -> IO a"
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
               ,"newDBC :: ResourceId -> IO DbcDesc"
               ,"newDBC rid = do"
               ,"  bs <- findFile rid"
               ,"  let [nr,nf,sr,ss] = runGet (sequence $ take 4 $ repeat getUInt) bs"
               ,"      dat = BS.take (sr*nr) $ BS.drop 16 bs"
               ,"      str = BS.take ss $ BS.drop (16 + sr*nr) bs"
               ,"  return $ DbcDesc (fromIntegral nr) (fromIntegral sr) dat str" ]

                                
{--
data DbcDesc = DbcDesc{ dbc_rnumber :: !Word32
                      , dbc_rsize   :: !Word32
                      , dbc_data    :: BS.ByteString
                      , dbc_strings :: BS.ByteString
                      }

data Record = Record{ rec_dbc    :: DbcDesc
                    , rec_offset :: !Word32
                    }

newDBC :: ResourceId -> IO DbcDesc
newDBC rid = do
  bs <- findFile rid
  let [nr,nf,sr,ss] = runGet (sequence $ take 4 $ repeat getUInt) bs
      dat = BS.take (sr*nr) $ BS.drop 16 bs
      str = BS.take ss $ BS.drop (16 + sr*nr) bs
  return $ DbcDesc (fromIntegral nr) (fromIntegral sr) dat str

getRecords :: DbcDesc -> [Record]
getRecords dbc@(DbcDesc rn rs dat str) = map (\i -> Record dbc (rs*i)) [1..rn]

--getInt :: (DBC d, f ~ Field d) => d -> f -> Int
--getInt dbc field = 

class DBC a where
    data Field a
    record :: a -> Field a -> Record
    new    :: ResourceId -> IO a


declDBC typName fields = do
  [declNew] <- [d|new r = newDBC r >>= return . $(conE name) |]
  declRec   <- let d = mkName "d"
                   r = mkName "r"
                   a = mkName "a"
                   e = [|getRecords $(varE a) !! $(return (CaseE (VarE $ mkName "r") alt))|]
               in  funD (mkName "record") 
                        [clause [varP d, varP r] 
                                (normalB (caseE (varE d)
                                                [match (conP name [varP a]) (normalB e) []])) 
                                []]
  return [ NewtypeD [] name [] (NormalC name [(NotStrict,ConT ''DbcDesc)]) []
         , InstanceD [] (AppT (ConT ''DBC) (ConT name)) [{--declField,--} declNew, declRec] ]
    where name = mkName typName
          alt  = map (\(n,v) -> Match (ConP (mkName n) []) (NormalB $ LitE $ IntegerL v) []) fields

--}

{--
data DbcDesc = DbcDesc
data Record  = Record
newDBC :: ResourceId -> IO DbcDesc
newDBC _ = return DbcDesc
getRecords :: DbcDesc -> [Record]
getRecords _ = []

data Field = F1 | F2 | F3

class DBC a where
    record :: a -> Field -> Record
    new    :: ResourceId -> IO a
--}


{--
newtype ItemDB = ItemDB DbcDesc
instance DBC ItemDB where
    data Field ItemDB = ID | Itemclass | Subclass | ItemDisplayInfo | InventorySlot | Sheath
    new r = 
        newDBC r >>= return . ItemDB
    record (ItemDB a) r = 
        getRecords a !! (case r of
                           ID        -> 0
                           Itemclass -> 1
                           Subclass  -> 2
                           ItemDisplayInfo -> 5
                           InventorySlot   -> 6
                           Sheath          -> 7)

newtype CharHairGeosetsDB = CharHairGeosetsDB DbcDesc
instance DBC CharHairGeosetsDB where
    data Filed CharHairGeosetsDB = CharHairGeosetID | Race | Gender | Section | Geoset
    new r = 
        newDBC r >>= return . CharHairGeosetID
    record (CharHairGeosetID a) r = 
        getRecords a !! (case r of
                           CharHairGeosetID -> 0
                           Race    -> 1
                           Gender  -> 2
                           Section -> 3
                           Geoset  -> 4)

--}