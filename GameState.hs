module GameState where

import Graphics.Rendering.OpenGL


data Sprite = Empty | Border
data Dir = DirIdle | DirUp | DirDown | DirLeft | DirRight
data Hero = Hero {pos :: (GLfloat, GLfloat), dir :: Dir}
data Ghost = Ghost

data GameState = GameState {hero :: Hero, ghosts :: [Ghost]}

heroSpeed :: GLfloat
heroSpeed = 0.01

sceneSize :: GLfloat
sceneSize = 60.0

quadSize :: GLfloat
quadSize = 3
