module GameState where

import Graphics.Rendering.OpenGL


data Sprite = Empty | Border
data Hero = Hero {pos :: (GLfloat, GLfloat)}
data Ghost = Ghost

data GameState = GameState {hero :: Hero, ghosts :: [Ghost]}
