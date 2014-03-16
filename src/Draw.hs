module Draw where

import GameState
import Graphics.Rendering.OpenGL hiding (Level)

class Drawable d where
	draw :: d -> Point2D -> IO ()
	draw _ p = drawQuad p [(0, 0), (0, quadSize), (quadSize, quadSize), (quadSize, 0)]

instance Drawable Sprite where
	draw LWall     p = drawPiece p $ drawLine [(offset, 0), (offset, quadSize)]
	draw RWall     p = drawPiece p $ drawLine [(offset, 0), (offset, quadSize)]
	draw UWall     p = drawPiece p $ drawLine [(0, offset), (quadSize, offset)]
	draw DWall     p = drawPiece p $ drawLine [(0, offset), (quadSize, offset)]
	draw UpLeft    p = drawPiece p $ drawLine [(offset, quadSize), (offset, offset+0.1), (offset+0.1, offset), (quadSize, offset)]
	draw UpRight   p = drawPiece p $ drawLine [(0, offset), (offset-0.1, offset), (offset, offset+0.1), (offset, quadSize)]
	draw DownLeft  p = drawPiece p $ drawLine [(offset, 0), (offset, offset-0.1), (offset+0.1, offset), (quadSize, offset)]
	draw DownRight p = drawPiece p $ drawLine [(0, offset), (offset-0.1, offset), (offset, offset-0.1), (offset, 0)]
	draw Bordr     p = drawQuad p [(0, 0), (0, quadSize), (quadSize, quadSize), (quadSize, 0)]
	draw Empty     _ = return ()

drawPiece :: Point2D -> IO () -> IO ()
drawPiece (x, y) drawFunc = do
	color levelColor
	preservingMatrix $ do
		translate $ Vector3 x y 0
		drawFunc

drawLevel :: [Sprite] -> IO ()
drawLevel = draw' 0
	where
		draw' n (h:r) = do
			draw h $ tile2point (fromIntegral $ n `rem` levelWidth, fromIntegral $ n `div` levelWidth)
			draw' (succ n) r
		draw' _ [] = return ()

drawHero :: Hero -> IO ()
drawHero Hero{pos = (x, y), stamp = (st, _), dir = d} = do
	color heroColor
	preservingMatrix $ do
		translate $ Vector3 x y 0
		rotate (angleFromDir d) $ Vector3 (0::GLfloat) 0 1
		renderPrimitive Polygon $ do
			vertex $ Vertex3 (-0.2::GLfloat) 0 0
			mapM_ (\angle -> vertex $ Vertex3 (cheeseheadRadius * cos angle) (cheeseheadRadius * sin angle) 0) [st*0.1, st*0.1 + 0.1 .. 2*pi-st*0.1]
			vertex $ Vertex3 (-0.2::GLfloat) 0 0
	where
		angleFromDir :: Dir -> GLfloat
		angleFromDir DirUp = 270.0
		angleFromDir DirLeft = 180.0
		angleFromDir DirDown = 90.0
		angleFromDir _ = 0.0

drawQuad :: Point2D -> [Point2D] -> IO ()
drawQuad p@(x, y) ps = drawPiece p $ renderPrimitive Quads $
	mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) ps

drawLine :: [Point2D] -> IO ()
drawLine ps = renderPrimitive LineStrip $
	mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) ps
