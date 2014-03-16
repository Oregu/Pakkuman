module GameState where

import Graphics.Rendering.OpenGL


data Sprite = Empty | Bordr | LWall | RWall | UWall | DWall | UpLeft | UpRight | DownLeft | DownRight
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
	minY = minX
	maxY = maxX
	minX = minY
	maxX = maxY

instance Boundable Hero where
	minX _ = 0.0
	maxX _ = cheeseheadRadius * 2

instance Boundable Sprite where
	minX LWall = 0.0
	minX RWall = offset - 0.1
	minX _ = 0.0

	maxX LWall = offset + 0.1
	maxX RWall = quadSize
	maxX _ = 0.0

	minY UWall = offset - 0.1
	minY DWall = 0.0
	minY _ = 0.0

	maxY UWall = quadSize
	maxY DWall = offset + 0.1
	maxY _ = 0.0

tile2point :: Tile -> Point2D
tile2point (x, y) = (fromIntegral x*quadSize, fromIntegral y*quadSize)

point2tile :: Point2D -> Tile
point2tile (x, y) = (floor $ x/quadSize,  floor $ y/quadSize)

gameSpeed = 16 :: Int
heroSpeed = 0.09 :: GLfloat
heroStartPos = (14*quadSize, 23.5*quadSize)
sceneSize = 60.0
quadSize = 1.0
offset = quadSize * 0.5
cheeseheadRadius = 0.8
levelColor = Color3 0.13 0.13 0.87
heroColor = Color3 0.8 0.8 0.2
levelWidth = 28 :: Int
levelHeight = 31 :: Int
