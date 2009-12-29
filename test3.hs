module Main where

import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw
import qualified Graphics.UI.GLUT as GLUT
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.OpenGL
import Control.Monad.State
import qualified Data.Map as M
import Text.Printf

import Data.WOW.World
import Data.WOW.M2Model
import Data.WOW.GL.Resource
import Data.WOW.GL.ResourceLoader
import Data.WOW.GL.Mesh

canvas_width, canvas_height :: Num a => a
canvas_width  = 350
canvas_height = 250

main = do
  initGUI
  initGL

  world  <- newIORef (WorldState{ _resLibrary = (ResourceLibrary glResourceLoader M.empty) })

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

  label_id     <- labelNew (Just "ID:")
  label_length <- labelNew (Just "Length:")
  label_speed  <- labelNew (Just "Speed:")
  range        <- adjustmentNew 50 0 100 1 1 1
  scale_time   <- hScaleNew range

  set window [ containerChild := hbox ]
  set hbox   [ containerChild := canvas
             , containerChild := vbox ]
  set vbox   [ containerChild := cmb
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
                          )
                 )

  onRealize canvas (withGLDrawingArea canvas $ \_ -> do
                      putStrLn "on realize"
                      myInit world
                      myLoadModelInfomation world (cmb,list) label_id label_length label_speed range
                   )

  onExpose canvas  (\_ -> do
                      withGLDrawingArea canvas (\w ->  do
                                                  putStrLn "on expose"
                                                  myDisplay world
                                                  glDrawableSwapBuffers w
                                               )
                      return True
                   )
  
  -- timeoutAddFull (widgetQueueDraw canvas >> return True) priorityDefaultIdle 20
  
  widgetShowAll window
  mainGUI

  
myInit _ = do
  clearColor $= Color4 0.4 0.4 0.4 1
  clearDepth $= 1
  depthFunc  $= Just Less
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
  return ()

myLoadModelInfomation world (cmb,list) label_id label_length label_speed range = 
  withWorld world $ do Just (GLModel mdl msh) <- findResource mm
                       lift $ do -- fill in the model of combobox
                                 listStoreClear list
                                 mapM_ (listStoreAppend list) (zip [1::Int .. ] $ m_animations_ mdl)
                                 -- select the first item if any
                                 when (not $ null $ m_animations_ mdl)
                                      (comboBoxSetActive cmb 0)

myDisplay world = do
  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity
  gluLookAt 0.0 0.0 5.0
            0.0 2.0 0.0
            0.0 1.0 0.0
  withWorld world (do Just (GLModel _ m) <- findResource mm
                      renderAll m )

ma = "MPQ:World\\Expansion02\\Doodads\\Generic\\TUSKARR\\Tables\\TS_Long_Table_01.m2"
mb = "MPQ:world\\goober\\g_xmastree.m2"
mc = "MPQ:Character\\BloodElf\\Male\\BloodElfMale.m2"
md = "MPQ:creature\\chicken\\chicken.m2"

mm = md

withWorld :: IORef (WorldState res) -> World res a -> IO a
withWorld w0 action = do a     <- readIORef w0
                         (b,c) <- runStateT action a
                         writeIORef w0 c
                         return b