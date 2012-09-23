module Pacman where

import GameState
import Keys (keyboardCallback)

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import System.IO
import System.CPUTime
import Data.Time.Clock (getCurrentTime, diffUTCTime)

start :: IO ()
start = do
	(progname, _) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered]
	createWindow "Pakkuman game"
	windowSize $= Size 640 480
	gs <- newIORef defaultGameState
	displayCallback $= display gs
	addTimerCallback 0 (update gs)
	reshapeCallback $= Just reshape
	keyboardMouseCallback $= Just (keyboardCallback gs)
	mainLoop

defaultGameState :: GameState
defaultGameState = GameState {hero = Hero {pos = (1.0, 1.0), dir = DirIdle, stamp = (0, 1)}, ghosts = []}

reshape :: Size -> IO ()
reshape s@(Size w h) = do
	viewport $= (Position 0 0, s)
	loadIdentity
	let
		aspect = realToFrac $ fromIntegral w / fromIntegral h
		border = realToFrac $ sceneSize/2.0
	if w > h
		then ortho2D 0 (border*aspect) border 0
		else ortho2D 0 border (border/aspect) 0

display :: IORef GameState -> IO ()
display gs = do
	GameState {hero = h} <- get gs
	clear [ColorBuffer]
	drawLevel
	drawHero h
	swapBuffers
	flush

update :: IORef GameState -> IO ()
update gsRef = do
	startTime <- getCurrentTime

	modifyIORef gsRef updateGS
	postRedisplay Nothing

	endTime <- getCurrentTime
	let timeSleep = if timeDiff < gameSpeed then gameSpeed - timeDiff else 0
		where timeDiff = truncate (1000 * (diffUTCTime endTime startTime))

	addTimerCallback timeSleep $ update gsRef
		where
			updateGS g@GameState {hero = h@Hero {pos = (x, y), stamp = s, dir = DirUp}} = g {hero = h {pos = (x, y - heroSpeed), stamp = hextHeroStamp s}}
			updateGS g@GameState {hero = h@Hero {pos = (x, y), stamp = s, dir = DirDown}} = g {hero = h {pos = (x, y + heroSpeed), stamp = hextHeroStamp s}}
			updateGS g@GameState {hero = h@Hero {pos = (x, y), stamp = s, dir = DirLeft}} = g {hero = h {pos = (x - heroSpeed, y), stamp = hextHeroStamp s}}
			updateGS g@GameState {hero = h@Hero {pos = (x, y), stamp = s, dir = DirRight}} = g {hero = h {pos = (x + heroSpeed, y), stamp = hextHeroStamp s}}
			updateGS gs = gs

			hextHeroStamp (s, d)	| d == 1  && s < 6  = (s+1,  1)
									| d == 1  && s == 6 = (  5, -1)
									| d == -1 && s > 0  = (s-1, -1)
									| otherwise = (1, 1)

drawLevel :: IO ()
drawLevel = do
	level <- loadLevel
	draw 0 level
	where
		draw n (Empty:r) = draw (succ n) r
		draw n (h:r) = do
			drawSprite (fromIntegral $ n `rem` levelLength) (fromIntegral $ n `div` levelLength) h
			draw (succ n) r
		draw _ [] = return ()

drawHero :: Hero -> IO ()
drawHero Hero{pos = (x, y), stamp = (st, _), dir = d} = do
	color $ Color3 (0.8 :: GLfloat) 0.8 0.2
	preservingMatrix $ do
		translate $ Vector3 (x*quadSize) (y*quadSize :: GLfloat) 0
		rotate (angleFromDir d) $ Vector3 (0::GLfloat) 0 1
		renderPrimitive Polygon $ do
			vertex $ Vertex3 (0::GLfloat) 0 0
			mapM_ (\angle -> vertex $ Vertex3 (cheeseheadRadius * cos angle) (cheeseheadRadius * sin angle) 0) [st*0.1, st*0.1 + 0.1 .. 2*pi-st*0.1]
			vertex $ Vertex3 (0::GLfloat) 0 0
	where
		angleFromDir :: Dir -> GLfloat
		angleFromDir DirUp = 270.0
		angleFromDir DirLeft = 180.0
		angleFromDir DirDown = 90.0
		angleFromDir _ = 0.0

loadLevel :: IO [Sprite]
loadLevel = readFile "level.1" >>= \s -> return $ foldr sprite [] s
	where
		sprite :: Char -> [Sprite] -> [Sprite]
		sprite '-' spr = Border:spr
		sprite ' ' spr =  Empty:spr
		sprite  _  spr =        spr

drawSprite :: GLfloat -> GLfloat -> Sprite -> IO ()
drawSprite _ _ Empty = return ()
drawSprite x y Border = drawQuad x y (Color3 0 0 1)

drawQuad :: GLfloat -> GLfloat -> Color3 GLfloat -> IO ()
drawQuad x y col = do
	color col
	preservingMatrix $ do
		translate $ Vector3 (x*quadSize) (y*quadSize :: GLfloat) 0
		renderPrimitive Quads $ do
			vertex $ Vertex3 (0::GLfloat) 0 0
			vertex $ Vertex3 0 quadSize 0
			vertex $ Vertex3 quadSize quadSize 0
			vertex $ Vertex3 quadSize 0 0
