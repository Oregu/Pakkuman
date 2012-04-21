module Pacman where

import GameState
import Keys (keyboardCallback)

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Random
import Monad
import Data.IORef

start :: IO ()
start = do
	(progname, _) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered]
	createWindow "Pakkuman game"
	gs <- newIORef defaultGameState
	displayCallback $= display gs
	idleCallback $= Just (idle gs)
	reshapeCallback $= Just reshape
	keyboardMouseCallback $= Just (keyboardCallback gs)
	mainLoop

defaultGameState :: GameState
defaultGameState = GameState {hero = Hero {pos = (1.0, 1.0), dir = DirIdle}, ghosts = []}

reshape :: Size -> IO ()
reshape s@(Size w h) = do
	viewport $= (Position 0 0, s)
	loadIdentity
	let
		aspect = realToFrac $ (fromIntegral w)/(fromIntegral h)
		border = realToFrac $ (sceneSize)/2.0
	if (w > h)
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

idle :: IORef GameState -> IO ()
idle gsRef = do
	modifyIORef gsRef updateGS
	postRedisplay Nothing
		where
			updateGS GameState {hero = Hero {pos = (x, y), dir = DirUp}, ghosts = gs} = GameState {hero = Hero {pos = (x, y - heroSpeed), dir = DirUp}, ghosts = gs}
			updateGS GameState {hero = Hero {pos = (x, y), dir = DirDown}, ghosts = gs} = GameState {hero = Hero {pos = (x, y + heroSpeed), dir = DirDown}, ghosts = gs}
			updateGS GameState {hero = Hero {pos = (x, y), dir = DirLeft}, ghosts = gs} = GameState {hero = Hero {pos = (x - heroSpeed, y), dir = DirLeft}, ghosts = gs}
			updateGS GameState {hero = Hero {pos = (x, y), dir = DirRight}, ghosts = gs} = GameState {hero = Hero {pos = (x + heroSpeed, y), dir = DirRight}, ghosts = gs}
			updateGS gs = gs

drawLevel :: IO ()
drawLevel = do
	level <- loadLevel
	draw 0 level
	where
		draw n (Empty:r) = draw (succ n) r
		draw n (h:r) = do
			drawSquare (fromIntegral $ n `rem` 9) (fromIntegral $ n `div` 9) h
			draw (succ n) r
		draw _ [] = return ()

drawHero :: Hero -> IO ()
drawHero Hero{pos = (x, y)} = do
	color $ Color3 (0.8 :: GLfloat) 0.8 0.2
	preservingMatrix $ do
		translate $ Vector3 (x*quadSize) (y*quadSize :: GLfloat) 0
		renderPrimitive Quads $ do
			vertex $ Vertex3 (0::GLfloat) 0 0
			vertex $ Vertex3 0 quadSize 0
			vertex $ Vertex3 quadSize quadSize 0
			vertex $ Vertex3 quadSize 0 0

loadLevel :: IO [Sprite]
loadLevel = return [ Border, Border, Border, Border, Border, Border, Border, Border, Border
					,Border, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Border
					,Border, Empty, Border, Border, Border, Border, Border, Empty, Border
					,Border, Empty, Border, Empty, Border, Empty, Empty, Empty, Border
					,Border, Empty, Empty, Empty, Border, Empty, Border, Empty, Border
					,Border, Empty, Border, Border, Border, Border, Border, Empty, Border
					,Border, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Border
					,Border, Border, Border, Border, Border, Border, Border, Border, Border ]

spriteColor :: Sprite -> Color3 GLfloat
spriteColor Empty = Color3 0 0 0
spriteColor Border = Color3 0 0 1

drawSquare :: GLfloat -> GLfloat -> Sprite -> IO ()
drawSquare x y sp =
	preservingMatrix $ do
		translate $ Vector3 (x*quadSize) (y*quadSize :: GLfloat) 0
		drawSprite sp

drawSprite :: Sprite -> IO ()
drawSprite Empty = return ()
drawSprite Border = do
	color $ spriteColor Border
	renderPrimitive Quads $ do
		vertex $ Vertex3 (0::GLfloat) 0 0
		vertex $ Vertex3 0 quadSize 0
		vertex $ Vertex3 quadSize quadSize 0
		vertex $ Vertex3 quadSize 0 0
