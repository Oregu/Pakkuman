module Keys where

import Graphics.UI.GLUT
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Data.IORef

keyboardCallback :: IORef (GLfloat, GLfloat) -> Key -> KeyState -> Modifiers -> Position -> IO()
keyboardCallback _ (Char '\27') Down _ _ = exitWith ExitSuccess
keyboardCallback pos (Char '+') Down _ _ = do
	(x,y) <- get pos
	pos $= (x,y+3)
keyboardCallback _ _ _ _ _ = return ()
