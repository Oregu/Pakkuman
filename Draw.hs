module Draw where

import GameState
import Graphics.Rendering.OpenGL hiding (Level)

offset :: GLfloat
offset = quadSize * 0.5

class Drawable d where
	draw :: d -> GLfloat -> GLfloat -> IO ()
	draw _ x y = drawQuad x y [(0, 0), (0, quadSize), (quadSize, quadSize), (quadSize, 0)]

instance Drawable Sprite where
	draw HWall     x y = drawPiece x y $ drawLine [(0, offset), (quadSize, offset)]
	draw VWall     x y = drawPiece x y $ drawLine [(offset, 0), (offset, quadSize)]
	draw UpLeft    x y = drawPiece x y $ drawLine [(offset, quadSize), (offset, offset+0.1), (offset+0.1, offset), (quadSize, offset)]
	draw UpRight   x y = drawPiece x y $ drawLine [(0, offset), (offset-0.1, offset), (offset, offset+0.1), (offset, quadSize)]
	draw DownLeft  x y = drawPiece x y $ drawLine [(offset, 0), (offset, offset-0.1), (offset+0.1, offset), (quadSize, offset)]
	draw DownRight x y = drawPiece x y $ drawLine [(0, offset), (offset-0.1, offset), (offset, offset-0.1), (offset, 0)]
	draw Bordr     x y = drawQuad x y [(0, 0), (0, quadSize), (quadSize, quadSize), (quadSize, 0)]
	draw Empty     _ _ = return ()

drawPiece x y drawFunc = do
	color levelColor
	preservingMatrix $ do
		translate $ Vector3 (x*quadSize) (y*quadSize :: GLfloat) 0
		drawFunc

drawLevel :: [Sprite] -> IO ()
drawLevel level = do
	draw' 0 level
	where
		draw' n (h:r) = do
			draw h (fromIntegral $ n `rem` levelLength) (fromIntegral $ n `div` levelLength)
			draw' (succ n) r
		draw' _ [] = return ()

drawHero :: Hero -> IO ()
drawHero Hero{pos = (x, y), stamp = (st, _), dir = d} = do
	color $ Color3 (0.8 :: GLfloat) 0.8 0.2
	preservingMatrix $ do
		translate $ Vector3 (x*quadSize) (y*quadSize :: GLfloat) 0
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

drawQuad :: GLfloat -> GLfloat -> [Point2D] -> IO ()
drawQuad x y ps = drawPiece x y $ renderPrimitive Quads $
	mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) ps

drawLine :: [Point2D] -> IO ()
drawLine ps = renderPrimitive LineStrip $
	mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) ps
