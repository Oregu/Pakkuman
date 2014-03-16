module Pacman where

import GameState
import Draw
import Keys (keyboardCallback)

import Control.Applicative
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Data.Time.Clock (getCurrentTime, diffUTCTime)


start :: IO ()
start = do
	getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered]
	createWindow "Pakkuman game"
	windowSize $= Size 540 580
	lvl <- loadLevel
	gs <- newIORef $ defaultGameState lvl
	displayCallback $= display gs
	addTimerCallback 0 (update gs)
	reshapeCallback $= Just reshape
	keyboardMouseCallback $= Just (Keys.keyboardCallback gs)
	mainLoop

defaultGameState :: [Sprite] -> GameState
defaultGameState lvl = GameState {hero = Hero {pos = heroStartPos, dir = DirIdle, stamp = (0, 1)}, ghosts = [], level = lvl}

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
	GameState {hero = h, level = l} <- get gs
	clear [ColorBuffer]
	drawLevel l
	drawHero h
	swapBuffers
	flush

update :: IORef GameState -> IO ()
update gs = do
	startTime <- getCurrentTime

	modifyIORef gs updateGS
	postRedisplay Nothing

	endTime <- getCurrentTime
	let timeSleep = if timeDiff < gameSpeed then gameSpeed - timeDiff else 0
		where timeDiff = truncate (1000 * diffUTCTime endTime startTime)

	addTimerCallback timeSleep $ update gs

updateGS :: GameState -> GameState
updateGS g@GameState {level = l, hero = h@Hero {pos = (x, y), stamp = s, dir = DirUp}} = g {hero = h {pos = collide l (x, y - heroSpeed), stamp = nextHeroStamp s}}
updateGS g@GameState {level = l, hero = h@Hero {pos = (x, y), stamp = s, dir = DirDown}} = g {hero = h {pos = collide l (x, y + heroSpeed), stamp = nextHeroStamp s}}
updateGS g@GameState {level = l, hero = h@Hero {pos = (x, y), stamp = s, dir = DirLeft}} = g {hero = h {pos = collide l (x - heroSpeed, y), stamp = nextHeroStamp s}}
updateGS g@GameState {level = l, hero = h@Hero {pos = (x, y), stamp = s, dir = DirRight}} = g {hero = h {pos = collide l (x + heroSpeed, y), stamp = nextHeroStamp s}}
updateGS gs = gs

nextHeroStamp :: (GLfloat, Int) -> (GLfloat, Int)
nextHeroStamp (s, d) | d == 1  && s < 6  = (s+1,  1)
					 | d == 1  && s == 6 = (  5, -1)
					 | d == -1 && s > 0  = (s-1, -1)
					 | otherwise = (1, 1)

collide :: [Sprite] -> Point2D -> Point2D
collide lvl p@(x, y) = checkX . checkY $ p where
	checkX (x, y)
		| heroLeft >= quadSize * fromIntegral(tx - 1) + minX ltile &&
			heroLeft < quadSize * fromIntegral(tx - 1) + maxX ltile
		= (cheeseheadRadius + quadSize * fromIntegral(tx - 1) + maxX ltile, y)
		| heroRight > quadSize * fromIntegral(tx + 1) + minX rtile &&
			heroRight <= quadSize * fromIntegral(tx + 1) + maxX rtile
		= (quadSize * fromIntegral(tx + 1) - cheeseheadRadius + minX rtile, y)
		| otherwise = (x, y)
	checkY (x, y)
		| heroUp >= quadSize * fromIntegral(ty-1) + minY utile &&
			heroUp < quadSize * fromIntegral(ty-1) + maxY utile
		= (x, quadSize * fromIntegral(ty-1) + cheeseheadRadius + maxY utile)
		| heroDown > quadSize * fromIntegral(ty+1) + minY dtile &&
			heroDown <= quadSize * fromIntegral(ty+1) + maxY dtile
		= (x, quadSize * fromIntegral(ty+1) - cheeseheadRadius + minY dtile)
		| otherwise = (x, y)
	(tx, ty) = point2tile (x, y)
	left = ty * levelWidth + tx - 1
	ltile = lvl !! left
	rtile = lvl !! (left + 2)
	utile = lvl !! (left - levelWidth + 1)
	dtile = lvl !! (left + levelWidth + 1)
	heroLeft = x - cheeseheadRadius
	heroRight = x + cheeseheadRadius
	heroUp = y - cheeseheadRadius
	heroDown = y + cheeseheadRadius

loadLevel :: IO [Sprite]
loadLevel = foldr sprite [] <$> readFile "level.1"
	where
		sprite :: Char -> [Sprite] -> [Sprite]
		sprite 'u' spr = UWall:spr
		sprite 'd' spr = DWall:spr
		sprite 'l' spr = LWall:spr
		sprite 'r' spr = RWall:spr
		sprite 'F' spr = UpLeft:spr
		sprite '7' spr = UpRight:spr
		sprite 'L' spr = DownLeft:spr
		sprite 'J' spr = DownRight:spr
		sprite ' ' spr = Empty:spr
		sprite  _  spr =       spr
