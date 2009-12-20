module Main where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.GLU.Raw
import System.Exit
import M2Model
import GL.Mesh

main = do
  getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
  initialWindowSize  $= Size 300 300
  initialWindowPosition $= Position 0 0
  createWindow "M2"
  clearColor      $= Color4 0.5 0.5 0.5 1.0
  -- m <- newModel "../tmp/world/azeroth/burningsteppes/passivedoodads/rocks/lavarock01.m2" >>= newMesh
  m <- newModel "../tmp/world/goober/g_xmastree.m2" >>= newMesh
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
  gluLookAt 0.0 0.0 8.0
            0.0 2.0 0.0
            0.0 1.0 0.0
  renderMesh m
  swapBuffers