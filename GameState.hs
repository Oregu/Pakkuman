module GameState where

import Graphics.Rendering.OpenGL


data Sprite = Empty | Border
data Dir = DirIdle | DirUp | DirDown | DirLeft | DirRight
data Hero = Hero {pos :: (GLfloat, GLfloat), dir :: Dir, stamp :: (Float, Int)}
data Ghost = Ghost

data GameState = GameState {hero :: Hero, ghosts :: [Ghost]}

gameSpeed :: Int
gameSpeed = 40

heroSpeed :: GLfloat
heroSpeed = 0.1

sceneSize :: GLfloat
sceneSize = 60.0

quadSize :: GLfloat
quadSize = 3

cheeseheadRadius :: GLfloat
cheeseheadRadius = 1.2