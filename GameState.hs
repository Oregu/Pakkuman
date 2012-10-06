module GameState where

import Graphics.Rendering.OpenGL


data Sprite = Empty | Bordr | VWall | HWall | UpLeft | UpRight | DownLeft | DownRight
data Dir = DirIdle | DirUp | DirDown | DirLeft | DirRight
data Hero = Hero {pos :: Point2D, dir :: Dir, stamp :: (Float, Int)}
data Ghost = Ghost

data GameState = GameState {hero :: Hero, ghosts :: [Ghost], level :: [Sprite]}

type Point2D = (GLfloat, GLfloat)
type Tile = (Int, Int)

class Boundable b where
	minX :: b -> GLfloat
	minY :: b -> GLfloat
	maxX :: b -> GLfloat
	maxY :: b -> GLfloat
	minX _ = 0 :: GLfloat
	minY = minX
	maxX _ = quadSize
	maxY = maxX

instance Boundable Hero where
	maxX _ = cheeseheadRadius * 2

instance Boundable Sprite

tile2point :: Tile -> Point2D
tile2point (x, y) = (fromIntegral x*quadSize, fromIntegral y*quadSize)

gameSpeed :: Int
gameSpeed = 16

heroSpeed :: GLfloat
heroSpeed = 0.09

heroStartPos :: Point2D
heroStartPos = (14*quadSize, 23.5*quadSize)

sceneSize :: GLfloat
sceneSize = 60.0

quadSize :: GLfloat
quadSize = 1.0

cheeseheadRadius :: GLfloat
cheeseheadRadius = 0.8

levelColor :: Color3 GLfloat
levelColor = Color3 0.13 0.13 0.87

heroColor :: Color3 GLfloat
heroColor = Color3 0.8 0.8 0.2

levelWidth :: Int
levelWidth = 28

levelHeight :: Int
levelHeight = 31
