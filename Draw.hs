module Draw where

import GameState
import Graphics.Rendering.OpenGL

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

drawQuad :: GLfloat -> GLfloat -> [Point2D] -> IO ()
drawQuad x y ps = drawPiece x y $ renderPrimitive Quads $
	mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) ps

drawLine :: [Point2D] -> IO ()
drawLine ps = renderPrimitive LineStrip $
	mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) ps
