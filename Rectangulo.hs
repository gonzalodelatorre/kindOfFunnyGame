module Rectangulo where

import Graphics.Rendering.OpenGL
import Puntos

miRectangulo :: GLfloat -> GLfloat -> IO ()
miRectangulo width height =
 displayPoints [(w,h,0),(w,-h,0),(-w,-h,0),(-w,h,0)] Quads
 where
  w = width/2
  h = height/2

