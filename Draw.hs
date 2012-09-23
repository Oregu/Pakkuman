module Draw where

import GameState
import Graphics.Rendering.OpenGL

drawQuad :: GLfloat -> GLfloat -> IO ()
drawQuad x y = drawQuad' x y 0 0 0 quadSize quadSize quadSize quadSize 0

drawHWall :: GLfloat -> GLfloat -> IO ()
drawHWall x y = drawQuad' x y 0 a1 0 a2 quadSize a2 quadSize a1
		where
			a1 = quadSize * 0.4
			a2 = quadSize - quadSize * 0.4

drawVWall :: GLfloat -> GLfloat -> IO ()
drawVWall x y = drawQuad' x y a1 0 a1 quadSize a2 quadSize a2 0
	where
		a1 = quadSize * 0.4
		a2 = quadSize - quadSize * 0.4

drawQuad' :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
drawQuad' x y xa ya xb yb xc yc xd yd = do
	color levelColor
	preservingMatrix $ do
		translate $ Vector3 (x*quadSize) (y*quadSize :: GLfloat) 0
		renderPrimitive Quads $ do
			vertex $ Vertex3 xa ya 0
			vertex $ Vertex3 xb yb 0
			vertex $ Vertex3 xc yc 0
			vertex $ Vertex3 xd yd 0
