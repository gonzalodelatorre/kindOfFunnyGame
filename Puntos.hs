module Puntos where
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL


renderInWindow displayFunction = do
 (progName,_) <- getArgsAndInitialize
 createWindow progName
 displayCallback $= displayFunction
 mainLoop
 
displayPoints points primitiveShape = do
 renderAs primitiveShape points
 flush

renderAs :: PrimitiveMode -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderAs figure ps = renderPrimitive figure$makeVertexes ps

makeVertexes :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
makeVertexes = mapM_ (\(x,y,z)->vertex$Vertex3 x y z) 


