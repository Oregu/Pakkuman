module Pacman where

import GameState
import Draw
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
	windowSize $= Size 540 580
	level <- loadLevel
	gs <- newIORef $ defaultGameState level
	displayCallback $= display gs
	addTimerCallback 0 (update gs)
	reshapeCallback $= Just reshape
	keyboardMouseCallback $= Just (keyboardCallback gs)
	mainLoop

defaultGameState :: [Sprite] -> GameState
defaultGameState level = GameState {hero = Hero {pos = heroStartPos, dir = DirIdle, stamp = (0, 1)}, ghosts = [], level = level}

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
		where timeDiff = truncate (1000 * (diffUTCTime endTime startTime))

	addTimerCallback timeSleep $ update gs

updateGS :: GameState -> GameState
updateGS g@GameState {hero = h@Hero {pos = (x, y), stamp = s, dir = DirUp}} = g {hero = h {pos = collide (x, y - heroSpeed), stamp = nextHeroStamp s}}
updateGS g@GameState {hero = h@Hero {pos = (x, y), stamp = s, dir = DirDown}} = g {hero = h {pos = collide (x, y + heroSpeed), stamp = nextHeroStamp s}}
updateGS g@GameState {hero = h@Hero {pos = (x, y), stamp = s, dir = DirLeft}} = g {hero = h {pos = collide (x - heroSpeed, y), stamp = nextHeroStamp s}}
updateGS g@GameState {hero = h@Hero {pos = (x, y), stamp = s, dir = DirRight}} = g {hero = h {pos = collide (x + heroSpeed, y), stamp = nextHeroStamp s}}
updateGS gs = gs

nextHeroStamp :: (Float, Int) -> (Float, Int)
nextHeroStamp (s, d) | d == 1  && s < 6  = (s+1,  1)
					 | d == 1  && s == 6 = (  5, -1)
					 | d == -1 && s > 0  = (s-1, -1)
					 | otherwise = (1, 1)

collide :: Point2D -> Point2D
collide (x, y) = (x, y)

loadLevel :: IO [Sprite]
loadLevel = readFile "level.1" >>= \s -> return $ foldr sprite [] s
	where
		sprite :: Char -> [Sprite] -> [Sprite]
		sprite '-' spr = HWall:spr
		sprite '|' spr = VWall:spr
		sprite 'F' spr = UpLeft:spr
		sprite '7' spr = UpRight:spr
		sprite 'L' spr = DownLeft:spr
		sprite 'J' spr = DownRight:spr
		sprite ' ' spr = Empty:spr
		sprite  _  spr =       spr
