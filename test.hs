module Main where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw
import System.Exit
import System.IO.Unsafe
import Data.IORef
import qualified Data.Map as M
import Control.Monad.State

import Data.WOW.M2Model
import Data.WOW.World
import Data.WOW.GL.Resource
import Data.WOW.GL.ResourceLoader
import Data.WOW.GL.Mesh(renderMesh)
import Data.WOW.BLP
import Data.WOW.GL.Types
import Data.WOW.GL.Texture


world = unsafePerformIO $ newIORef undefined

main = do
  getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode, WithAlphaComponent, WithDepthBuffer]
  initialWindowSize  $= Size 300 300
  initialWindowPosition $= Position 0 0
  createWindow "M2"

  let ma = "MPQ:World\\Expansion02\\Doodads\\Generic\\TUSKARR\\Tables\\TS_Long_Table_01.m2"
  let mb = "MPQ:world\\goober\\g_xmastree.m2"
  let mc = "MPQ:Character\\BloodElf\\Male\\BloodElfMale.m2"
  let md = "MPQ:creature\\chicken\\chicken.m2"
  (GLMesh m,w0) <- runStateT (loadResource md) beginning
  world $= w0

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
  cullFace  $= Nothing
  -- polygonMode $= (Line, Line)

  swapBytes    Pack $= False
  lsbFirst     Pack $= False
  rowAlignment Pack $= 1

  displayCallback $= render m
  idleCallback    $= Just (render m)
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboard
  mainLoop

reshape (Size w h) = do
  viewport   $= (Position 0 0, Size w h)
  matrixMode $= Projection
  loadIdentity
  gluPerspective 45 (fromIntegral w / fromIntegral h) 1 1000
  matrixMode $= Modelview 0
  loadIdentity

keyboard key Down _ _ = do
  case key of
    Char '\ESC' -> exitSuccess
    Char x   -> putStrLn $ "Pressed " ++ show x
    _  -> return ()
keyboard _ _ _ _ = return ()

render m = do
  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity
  gluLookAt 0.0 0.0 5.0
            0.0 2.0 0.0
            0.0 1.0 0.0
  w <- readIORef world
  w <- execStateT (renderMesh m) w
  writeIORef world w
  swapBuffers