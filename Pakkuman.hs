module Pacman where

import Keys (keyboardCallback)

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Random
import Monad
import Data.IORef

data Sprite = Empty | Border | Item | Player

start = do
	(progname, _) <- getArgsAndInitialize
	createWindow "Pacman World"
	pacmanPosition <- newIORef (1.0, 1.0)
	displayCallback $= display pacmanPosition
	idleCallback $= Just (display pacmanPosition)
	reshapeCallback $= Just reshape
	keyboardMouseCallback $= Just (keyboardCallback pacmanPosition)
	mainLoop

sceneSize :: GLfloat
sceneSize = 60.0

quadSize :: GLfloat
quadSize = 3

reshape s@(Size w h) = do
	viewport $= (Position 0 0, s)
	loadIdentity
	let
		aspect = realToFrac $ (fromIntegral w)/(fromIntegral h)
		border = realToFrac $ (sceneSize)/2.0
	if (w > h)
		then ortho2D 0 (border*aspect) border 0
		else ortho2D 0 border (border/aspect) 0

display :: IORef (GLfloat, GLfloat) -> IO ()
display pos = do
	clear [ColorBuffer]
	drawLevel
	drawHero pos
	flush

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

drawHero :: IORef (GLfloat, GLfloat) -> IO ()
drawHero p = do
	(x,y) <- get p
	drawSquare x y Player

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
spriteColor Item = Color3 0.2 0.2 0.2
spriteColor Player = Color3 0.8 0.8 0.2

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

drawSprite Player = do
	color $ spriteColor Player
	renderPrimitive Quads $ do
		vertex $ Vertex3 (0::GLfloat) 0 0
		vertex $ Vertex3 0 quadSize 0
		vertex $ Vertex3 quadSize quadSize 0
		vertex $ Vertex3 quadSize 0 0

