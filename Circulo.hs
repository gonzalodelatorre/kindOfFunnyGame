module Circulo where
import Puntos
import Graphics.Rendering.OpenGL

circlePoints :: (Enum t, Floating t, Num t1) => t -> t -> [(t, t, t1)]
circlePoints radius number
 = [let alpha = twoPi * i /number
   in (radius*(sin (alpha)) ,radius * (cos (alpha)),0)
  |i <- [1,2..number]]
  where
   twoPi = 2*pi

circle :: (Enum t, Floating t, Num t1) => t -> [(t, t, t1)]   
circle radius = circlePoints radius 100   


fillCircle r = displayPoints (circle r) Polygon