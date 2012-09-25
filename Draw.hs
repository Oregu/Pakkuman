module Draw where

import GameState
import Graphics.Rendering.OpenGL

offset :: GLfloat
offset = quadSize * 0.4

drawSquare :: GLfloat -> GLfloat -> IO ()
drawSquare x y = drawQuad x y [(0, 0), (0, quadSize), (quadSize, quadSize), (quadSize, 0)]

drawHWall :: GLfloat -> GLfloat -> IO ()
drawHWall x y = drawLevelPiece x y $
	drawLine [(0, offset), (quadSize, offset)]

drawVWall :: GLfloat -> GLfloat -> IO ()
drawVWall x y = drawLevelPiece x y $
	drawLine [(offset, 0), (offset, quadSize)]

drawUpLeft :: GLfloat -> GLfloat -> IO ()
drawUpLeft x y = drawLevelPiece x y $ do
	drawLine [(offset, offset), (offset, quadSize), (offset, offset), (quadSize, offset)]

drawUpRight :: GLfloat -> GLfloat -> IO ()
drawUpRight x y = drawLevelPiece x y $
	drawLine [(0, offset), (offset, offset), (offset, offset), (offset, quadSize)]

drawDownLeft :: GLfloat -> GLfloat -> IO ()
drawDownLeft x y = drawLevelPiece x y $
	drawLine [(offset, 0), (offset, offset), (offset, offset), (quadSize, offset)]

drawDownRight :: GLfloat -> GLfloat -> IO ()
drawDownRight x y = drawLevelPiece x y $ do
	drawLine [(offset, 0), (offset, offset), (offset, offset), (0, offset)]

drawLevelPiece x y drawFunc = do
	color levelColor
	preservingMatrix $ do
		translate $ Vector3 (x*quadSize) (y*quadSize :: GLfloat) 0
		drawFunc

drawQuad :: GLfloat -> GLfloat -> [Point2D] -> IO ()
drawQuad x y ps = drawLevelPiece x y $ renderPrimitive Quads $
	mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) ps

drawLine :: [Point2D] -> IO ()
drawLine ps = renderPrimitive Lines $
	mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) ps
