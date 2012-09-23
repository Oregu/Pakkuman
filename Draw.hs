module Draw where

import GameState
import Graphics.Rendering.OpenGL

offset :: GLfloat
offset = quadSize * 0.4

drawSquare :: GLfloat -> GLfloat -> IO ()
drawSquare x y = drawQuad x y 0 0 0 quadSize quadSize quadSize quadSize 0

drawHWall :: GLfloat -> GLfloat -> IO ()
drawHWall x y = drawLevelPiece x y $
	drawLine 0 offset quadSize offset

drawVWall :: GLfloat -> GLfloat -> IO ()
drawVWall x y = drawLevelPiece x y $
	drawLine offset 0 offset quadSize

drawUpLeft :: GLfloat -> GLfloat -> IO ()
drawUpLeft x y = drawLevelPiece x y $ do
	drawLine offset offset offset quadSize
	drawLine offset offset quadSize offset

drawUpRight :: GLfloat -> GLfloat -> IO ()
drawUpRight x y = drawLevelPiece x y $ do
	drawLine 0 offset offset offset
	drawLine offset offset offset quadSize

drawDownLeft :: GLfloat -> GLfloat -> IO ()
drawDownLeft x y = drawLevelPiece x y $ do
	drawLine offset 0 offset offset
	drawLine offset offset quadSize offset

drawDownRight :: GLfloat -> GLfloat -> IO ()
drawDownRight x y = drawLevelPiece x y $ do
	drawLine offset 0 offset offset
	drawLine offset offset 0 offset

drawLevelPiece x y drawFunc = do
	color levelColor
	preservingMatrix $ do
		translate $ Vector3 (x*quadSize) (y*quadSize :: GLfloat) 0
		drawFunc

drawQuad :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
drawQuad x y xa ya xb yb xc yc xd yd = do
	drawLevelPiece x y $ renderPrimitive Quads $ do
		vertex $ Vertex3 xa ya 0
		vertex $ Vertex3 xb yb 0
		vertex $ Vertex3 xc yc 0
		vertex $ Vertex3 xd yd 0

drawLine :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
drawLine x1 y1 x2 y2 = do
	renderPrimitive Lines $ do
		vertex $ Vertex3 x1 y1 0
		vertex $ Vertex3 x2 y2 0
