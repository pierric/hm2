{-# LANGUAGE FlexibleInstances #-}
module Main where

import Graphics.Rendering.OpenGL.GL as GL -- hiding (scale,rotate)
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw
import qualified Graphics.UI.GLUT as GLUT
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.OpenGL
import Control.Monad.State
import qualified Data.Map as M
import Text.Printf
import Control.Arrow((***))
import Data.Array
import Data.VectorSpace
import Data.Maybe(catMaybes, fromJust)
import System.FilePath.Windows
import qualified Control.Monad.State.Lazy as S

import Data.WOW.World
import Data.WOW.ModelDef
import Data.WOW.M2Model
import Data.WOW.Creature 
import Data.WOW.Matrix
import Data.WOW.Bone
import Data.WOW.FileSystem
import Data.WOW.Utils
import Data.WOW.Animated
import Data.WOW.Quaternion
import Data.WOW.GL.Types
import Data.WOW.GL.ResourceLoader
import Data.WOW.GL.Mesh

canvas_width, canvas_height :: Num a => a
canvas_width  = 700
canvas_height = 500

type MyWorldState = WorldState World MockMPQ GLResource
newtype World a = World { unWorld :: S.StateT MyWorldState IO a }

instance M2World World MockMPQ GLResource where
    getWorld = World S.get
    modWorld = World . S.modify
instance Monad World where
    return = World . return
    a >>= f = World (unWorld a >>= (\v -> unWorld (f v)))
instance MonadIO World where
    liftIO = World . liftIO

main = do
  GLUT.getArgsAndInitialize
  initGUI
  initGL

  world  <- newIORef (WorldState{ _filesystem = MockMPQ ["..","tmp"]
                                , _resLibrary = ResourceLibrary glResourceLoader M.empty
                                , _db_creaturemodel = Nothing 
                                , _db_creatureskin  = Nothing } :: MyWorldState)
  mass   <- newIORef (-1,0)
  crea   <- newIORef undefined

  config <- glConfigNew [GLModeRGBA, GLModeAlpha, GLModeDepth, GLModeDouble]
  canvas <- glDrawingAreaNew config
  widgetSetSizeRequest canvas canvas_width canvas_height

  -- create window
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 8
             , windowTitle := "Animation" ]

  -- a hbox, left of which is glcanvas, right of which is configurations
  hbox  <- hBoxNew False 8
  vbox  <- vBoxNew False 8

  -- a combo box
  -- 1st. Model is ListStore [Animatioin]
  list  <- listStoreNew []
  -- 2nd. View
  cmb   <- comboBoxNewWithModel list
  -- 3rd. Renderer
  cmb_renderer <- cellRendererTextNew
  cellLayoutPackStart cmb cmb_renderer True
  cellLayoutSetAttributes cmb cmb_renderer list (\(idx,ani) -> [ cellText := printf "Anim:%2d" idx ])

  skinSel <- comboBoxNewText 

  label_id     <- labelNew (Just "ID:")
  label_length <- labelNew (Just "Length:")
  label_speed  <- labelNew (Just "Speed:")
  range        <- adjustmentNew 50 0 100 1 1 1
  scale_time   <- hScaleNew range

  set window [ containerChild := hbox ]
  set hbox   [ containerChild := canvas
             , containerChild := vbox ]
  set vbox   [ containerChild := cmb
             , containerChild := skinSel
             , containerChild := label_id
             , containerChild := label_length
             , containerChild := label_speed
             , containerChild := scale_time ]

  -- If an new item in combobox is selected, update each label and the scale.
  on cmb changed (do size <- listStoreGetSize list
                     idx  <- comboBoxGetActive cmb
                     when (size > 0 && idx >=0)
                          (do (_,anim) <- listStoreGetValue list idx
                              let id   = printf "%d-%d"  (anim_Id_ anim) (anim_subId_ anim)
                                  len  = anim_length_ anim
                                  spd  = printf "%.2f" (anim_move_speed_  anim)
                              set label_id     [ labelText := "ID:" ++ id ]
                              set label_length [ labelText := "Length:"++ show len ]
                              set label_speed  [ labelText := "Speed:" ++ spd ]
                              set range [ adjustmentUpper := fromIntegral len
                                        , adjustmentValue := 0
                                        , adjustmentStepIncrement := (fromIntegral len / 100)
                                        , adjustmentPageIncrement := (fromIntegral len / 100)
                                        , adjustmentPageSize := 1 ] 
                              widgetQueueDraw canvas
                          )
                     writeIORef mass (idx,0)
                 )
  
  on skinSel changed (do idx <- comboBoxGetActive skinSel
                         c   <- readIORef crea
                         c'  <- withWorld world (creatureChangeSkin c idx)
                         writeIORef crea c'
                         widgetQueueDraw canvas
                     )

  onValueChanged range (do v <- Graphics.UI.Gtk.get range adjustmentValue
                           modifyIORef mass (id *** const (ceiling v))
                           widgetQueueDraw canvas)

  onRealize canvas (withGLDrawingArea canvas $ \_ -> do
                      putStrLn "on realize"
                      myInit world crea (cmb, list) skinSel
                   )

  onExpose canvas  (\_ -> do
                      withGLDrawingArea canvas (\w ->  do
                                                  myDisplay world mass crea
                                                  glDrawableSwapBuffers w
                                               )
                      return True
                   )
  
  --timeoutAddFull (widgetQueueDraw canvas >> return True) priorityDefaultIdle 20
  
  widgetShowAll window
  mainGUI

  
myInit world creaR (animSel,animM) skinSel = do
  clearColor $= Color4 0.4 0.4 0.4 1
  clearDepth $= 1
  depthFunc  $= Just Less
  blend      $= Disabled
  shadeModel $= Smooth
  glEnable gl_TEXTURE_2D
  lighting   $= Enabled
  ambient  (Light 1) $= Color4 1.0 1.0 1.0 1.0
  diffuse  (Light 1) $= Color4 1.0 1.0 1.0 1.0
  position (Light 1) $= Vertex4 0 0 5 (1.0 :: GLfloat)
  light    (Light 1) $= Enabled
  hint PerspectiveCorrection $= Nicest
  matrixMode $= Projection
  loadIdentity
  gluPerspective 45 (canvas_width / canvas_height) 1 1000
  matrixMode $= Modelview 0
  loadIdentity
  
  withWorld world $ do crea <- newCreature mm (0 :: Int)
                       liftIO $ writeIORef creaR crea
                       sl   <- creatureSkinList (_crea_name crea)
                       Just (GLModel mdl msh) <- findResource (_crea_resource crea)

                       -- print all textures
                       liftIO $ do mapM_ (\t -> case t of
                                                  Data.WOW.M2Model.Texture f _ -> putStrLn f
                                                  _ -> return ()
                                         ) (m_textures_ mdl)
                                   mapM_ (putStrLn . drop 4) (catMaybes $ concat sl)

                       liftIO $ do -- fill in the model of combobox
                                   listStoreClear animM
                                   mapM_ (listStoreAppend animM) (zip [1::Int .. ] $ m_animations_ mdl)
                                   -- select the first item if any
                                   when (not $ null $ m_animations_ mdl)
                                        (comboBoxSetActive animSel 0)
                                   mapM_ (comboBoxAppendText skinSel . takeFileName . head . catMaybes) sl
                                   when (not $ null $ sl)
                                        (comboBoxSetActive skinSel 0)
  putStrLn "Initialization Done.."

myDisplay world mass creaR = do
  (anim,time) <- readIORef mass
  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity
  gluLookAt 0.0 0.0 30
            0.0 5.0 0.0
            0.0 1.0 0.0
  GL.rotate (90 :: GLfloat) (Vector3 (-1) 0 0)
  view <- (GLUT.get $ matrix $ Just $ Modelview 0 :: IO (GLmatrix GLfloat)) >>= getMatrixComponents RowMajor
  withWorld world (do crea <- liftIO $ readIORef creaR 
                      Just (GLModel mdl msh) <- findResource (_crea_resource crea)
                      case () of 
                        _
                          | anim < 0  -> renderAll [] msh
                          | otherwise
                              -> -- lift $ drawBone' (fromList view) anim time (m_bones_ mdl)
                                 let matrix = transform (fromList view) anim time (m_bones_ mdl)
                                 in  skeletonAnim matrix msh >>= renderAll (_crea_skin crea)  >> drawBounding mdl
                  )

drawBone' view anim time bones
    = let matrix = transform view anim time bones
          pivots = zipWith multVec3 matrix (map bone_pivot_ bones)
      in  drawBone $ map (\(bn,np) -> bn{ bone_pivot_ = np}) $ zip bones pivots
    
drawBone bones
    = do depthFunc $= Nothing
         lighting  $= Disabled
         GLUT.lineWidth $= 2
         color (Color3 (gf 0) 0 0)
         renderPrimitive Lines (mapM_ (\(idx,b0)-> 
                                           let (Vector3 a b c) = bone_pivot_ b0
                                               par = bone_parent_ (bones !! idx)
                                           in  when (par /= -1)
                                                    (let Vector3 e f g = bone_pivot_ (bones !! par)
                                                     in  vertex (Vertex3 (gf a) (gf b) (gf c)) >>
                                                         vertex (Vertex3 (gf e) (gf f) (gf g)))
                                      ) $ zip [0..] bones)
         GLUT.pointSize $= 3
         renderPrimitive Points (mapM_ (\(idx,b0)-> 
                                            let (Vector3 a b c) = bone_pivot_ b0
                                                par = bone_parent_ (bones !! idx)
                                            in  when (par == -1)
                                                     (vertex (Vertex3 (gf a) (gf b) (gf c)))
                                       ) $ zip [0..] bones)
         GLUT.pointSize $= 10
         let Vector3 x0 y0 z0 = bone_pivot_ $ head bones
         color (Color3 (gf 1) 0 0)
         renderPrimitive Points (vertex (Vertex3 (gf x0) (gf y0) (gf z0)))

drawBounding mdl = {-- lift $ do GLUT.pointSize $= 3
                             let (is,vs) = m_bounding_volume_ mdl
                             renderPrimitive Triangles (mapM_ (\idx -> let (Vector3 a b c) = vs !! idx 
                                                                       in  vertex (Vertex3 (gf a) (gf b) (gf c))) is) --}
                   -- lift $ GLUT.renderObject GLUT.Wireframe (GLUT.Sphere' (realToFrac $ vertexRadius_ $ m_bounding_ mdl) 10 10)
                   liftIO $ GLUT.renderObject GLUT.Wireframe (GLUT.Sphere' (realToFrac $ vertexRadius_ $ m_bounding_ mdl) 10 10)

gf = realToFrac :: Float -> GLfloat

ma = "World\\Expansion02\\Doodads\\Generic\\TUSKARR\\Tables\\TS_Long_Table_01"
mb = "world\\goober\\g_xmastree"
mc = "Character\\BloodElf\\Male\\BloodElfMale"
md = "Creature\\Chicken\\Chicken"
me = "Creature\\Cat\\Cat"
mf = "Creature\\Chimera\\Chimera"
mg = "Creature\\Dragon\\Onyxiamount"
mh = "Creature\\BloodElfGuard\\BloodElfMale_Guard"
mi = "Creature\\AncientOfWar\\AncientofWar"
mm = mf

withWorld :: IORef MyWorldState -> World a -> IO a
withWorld w0 action = do a     <- readIORef w0
                         (b,c) <- runStateT (unWorld action) a
                         writeIORef w0 c
                         return b
{--
printMatrix = do
  m <- newModel mm
  let t = transform identity4 0 1000 (m_bones_ m)
  writeFile "out" $ unlines $ map pm t
--}

p bone = bone_pivot_ bone
t bone = at (bone_tran_ bone) 0 1000 (Vector3 0 0 0)
r bone = at (bone_rota_ bone) 0 1000 identityQ
s bone = at (bone_scal_ bone) 0 1000 (Vector3 1 1 1)

mat func bone = foldl func identity4 [ translation (p bone)
                                     , translation (t bone)
                                     , Data.WOW.Quaternion.rotate (r bone)
                                     , Data.WOW.Matrix.scale (s bone)
                                     , translation (negateV (p bone))]

pm m = printf "[%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f]"
               (m!(0,0)) (m!(0,1)) (m!(0,2)) (m!(0,3))
               (m!(1,0)) (m!(1,1)) (m!(1,2)) (m!(1,3))
               (m!(2,0)) (m!(2,1)) (m!(2,2)) (m!(2,3))
               (m!(3,0)) (m!(3,1)) (m!(3,2)) (m!(3,3))

-- g b1 = zip (a_times_ (bone_rota_ b1) !! 0 ) (a_data_  (bone_rota_ b1) !! 0) 
