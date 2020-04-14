import Circulo
import Rectangulo
import Puntos



import Graphics.Rendering.OpenGL
import Data.IORef
import Graphics.UI.GLUT as GLUT




_LEFT = -2
_RIGHT = 1
_TOP = 1
_BOTTOM= -1

_POSICION_X_INICIAL = -0.6
_POSICION_Y_INICIAL = -1

largoPaleta = 0.07
anchoPaleta = 0.2
radioPelota = 0.035

_INITIAL_WIDTH :: GLsizei
_INITIAL_WIDTH=400

_INITIAL_HEIGHT::GLsizei
_INITIAL_HEIGHT=200

_INITIAL_BALL_DIR = -0.002
_INITIAL_PADDLE_DIR = 0.005



_INITIAL_BALL_DIR2 = -0.002
_INITIAL_BALL_DIR3 = -0.002


data Pelota = Pelota (GLfloat,GLfloat) GLfloat GLfloat

type Paleta = (GLfloat,GLfloat,GLfloat)

data Juego
 = Juego { pelota1 :: Pelota,
           pelota2 :: Pelota,
		   pelota3 :: Pelota
 , paleta  :: Paleta
 
 , movimientoPelota1::GLfloat
 , movimientoPelota2::GLfloat
 , movimientoPelota3::GLfloat}


initGame
 = Juego {  pelota1=   Pelota (0.9,0.9) (_INITIAL_BALL_DIR) (_INITIAL_BALL_DIR),
           pelota2 = Pelota (-1.8,0.9) ( _INITIAL_BALL_DIR2) (_INITIAL_BALL_DIR2),
		   pelota3 = Pelota (0,0.9) ( _INITIAL_BALL_DIR3) (_INITIAL_BALL_DIR3)
        ,  paleta=(_POSICION_X_INICIAL,_POSICION_Y_INICIAL,0)
        ,   movimientoPelota1 = 0.39
        ,   movimientoPelota2 = 0.36
        ,   movimientoPelota3 = 0.2 } 

main = do
 (progName,_) <- getArgsAndInitialize
 initialDisplayMode $= [DoubleBuffered]
 createWindow progName
 juego <- newIORef initGame
 windowSize $= Size _INITIAL_WIDTH _INITIAL_HEIGHT
 fullScreen
 displayCallback $= display juego
 idleCallback $= Just (idle juego)
 keyboardMouseCallback $= Just (keyboard juego)
 reshapeCallback $= Just (reshape juego)
 mainLoop

 
display juego = do
 clear [ColorBuffer]
 g <- get juego
 let (Pelota pos xDir yDir) = pelota1 g
     (Pelota pos' xDir' yDir') = pelota2 g
     (Pelota pos'' xDir'' yDir'') = pelota3 g
 displayPelota pos ( fillCircle radioPelota ) (Color4 1 0 0 1)
 displayPelota pos' ( fillCircle radioPelota) (Color4 0 0 1 1)
 displayPelota pos'' (fillCircle radioPelota) (Color4 0 1 1 1)
 displayPaleta$paleta g
 swapBuffers

displayPaleta (x,y,_) = do
 translate (Vector3 (largoPaleta/2) (anchoPaleta/2) 0)
 displayAt (x,y)$miRectangulo  anchoPaleta largoPaleta 
 

displayAt (x, y) displayMe =  do
 currentColor $= Color4 0 1 0 1
 translate$Vector3 x y (0::GLfloat)
 displayMe 
 loadIdentity
 
 
displayPelota (x, y) displayMe color =  do
 currentColor $= color
 translate$Vector3 x y (0::GLfloat)
 displayMe 
 loadIdentity 

 
idle juego = do
 g <- get juego
 let fac =  movimientoPelota1 g
 juego $= g {pelota1 = moverPelota g,
            pelota2= moverPelota2 g,
			pelota3 = moverPelota3 g
		   ,paleta = moverPaleta (paleta g) fac
       
           }
 postRedisplay Nothing 



 
moverPelota2 g
 = Pelota (x+0.2*factor*newXDir,y+factor*newYDir) newXDir newYDir
 where
  newXDir
   |x <= _LEFT-radioPelota = -xDir
   |x >= _RIGHT+radioPelota = -xDir
   |otherwise = xDir
  newYDir
   |y <= _BOTTOM - radioPelota= 0  
   |y-radioPelota <= yl+largoPaleta && x+radioPelota >= xl && x <=xl+anchoPaleta = -yDir
   |y > _TOP - radioPelota = -yDir
   |newXDir == 0 = 0
   |otherwise = yDir 
  (Pelota (x,y) xDir yDir) = pelota2 g
  factor =  movimientoPelota2 g
  (xl,yl,_) = paleta g

 
moverPelota3 g = Pelota (x+factor*newXDir,y+0.5*factor*newYDir) newXDir newYDir
 where
  newXDir
   |x <= _LEFT-radioPelota = -xDir
   |x >= _RIGHT+radioPelota = -xDir
   |otherwise = xDir
  newYDir
   |y <= _BOTTOM - radioPelota= 0
   |y-radioPelota <= yl+largoPaleta && x+radioPelota >= xl && x <=xl+anchoPaleta = -yDir
   |y > _TOP-radioPelota  = -yDir  
   |newXDir == 0 = 0
   |otherwise = yDir 
  (Pelota (x,y) xDir yDir) = pelota3 g
  factor =  movimientoPelota3 g
  (xl,yl,_) = paleta g
  
 
  
moverPelota g
 = Pelota (x+factor*newXDir,y+factor*newYDir) newXDir newYDir
 where
  newXDir
   |x <= _LEFT-radioPelota = -xDir
   |x >= _RIGHT+radioPelota = -xDir
   |otherwise = xDir
  newYDir
   |y <= _BOTTOM - radioPelota= 0 
   |y-radioPelota <= yl+largoPaleta && x+radioPelota >= xl && x <=xl+anchoPaleta = -yDir
   |y > _TOP-radioPelota = -yDir  
   |newXDir == 0 = 0
   |otherwise = yDir 
  (Pelota (x,y) xDir yDir) = pelota1 g
  factor =  movimientoPelota1 g
  (xl,yl,_) = paleta g
 
 
moverPaleta (x,y,dir) factor =
 let x1 = x + factor*dir
     newX = min (_RIGHT-largoPaleta) $max _LEFT x1
 in (newX,y,dir) 
 
 
 
keyboard juego (Char 'a') Up _ _ = do
  g <- get juego
  let (x,y,z) = paleta g
  juego $= g{paleta=(x-0.1,y,z)}

keyboard juego (Char 's') Up _ _ = do
  g <- get juego
  let (x,y,z) = paleta g
  juego $= g{paleta=(x+0.1,y,z)}

  
 
  
keyboard juego (Char '\32') Up _ _ = do
  g <- get juego
  let Pelota (x,y) xD yD     = pelota1 g
      Pelota (x',y') xD' yD' = pelota2 g
      Pelota (x'',y'') xD'' yD'' = pelota3 g
  if (yD==0 && yD'==0 && yD''==0) then juego$=g{   pelota1=   Pelota (x,0.9) ( _INITIAL_BALL_DIR) (_INITIAL_BALL_DIR),
                                                          pelota2 = Pelota (x',0.9) ( _INITIAL_BALL_DIR2) (_INITIAL_BALL_DIR2),
		                                                pelota3 = Pelota (x'',0.9) (  _INITIAL_BALL_DIR3) (_INITIAL_BALL_DIR3) }
	                                         
				                 else return ()

keyboard _ _ _ _ _ = do return () 



reshape juego s@(Size w h) = do
 viewport $= (Position 0 0, s)
 matrixMode $= Projection
 loadIdentity
 ortho (-2.0) 1.0 (-1.0) 1.0 (-1.0) 1.0
 matrixMode $= Modelview 0