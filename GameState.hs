module GameState where

import Graphics.Rendering.OpenGL


data Sprite = Empty | Bordr | VWall | HWall
data Dir = DirIdle | DirUp | DirDown | DirLeft | DirRight
data Hero = Hero {pos :: (GLfloat, GLfloat), dir :: Dir, stamp :: (Float, Int)}
data Ghost = Ghost

data GameState = GameState {hero :: Hero, ghosts :: [Ghost], level :: [Sprite]}

gameSpeed :: Int
gameSpeed = 16

heroSpeed :: GLfloat
heroSpeed = 0.05

sceneSize :: GLfloat
sceneSize = 60.0

quadSize :: GLfloat
quadSize = 2

cheeseheadRadius :: GLfloat
cheeseheadRadius = 0.8

levelLength :: Int
levelLength = 14

levelColor :: Color3 GLfloat
levelColor = Color3 0 0 1