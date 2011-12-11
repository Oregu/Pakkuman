module Keys where

import Graphics.UI.GLUT
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Data.IORef

keyboardCallback :: IORef (GLfloat, GLfloat) -> Key -> KeyState -> Modifiers -> Position -> IO()
-- EXit keys
keyboardCallback _ (Char '\ESC') Down _ _ = exitWith ExitSuccess
-- Movement keys
keyboardCallback pos (SpecialKey KeyRight) Down _ _ = modifyIORef pos (\(x,y) -> (x+1,y))
keyboardCallback pos (SpecialKey KeyLeft) Down _ _ = modifyIORef pos (\(x,y) -> (x-1,y))
keyboardCallback pos (SpecialKey KeyUp) Down _ _ = modifyIORef pos (\(x,y) -> (x,y-1))
keyboardCallback pos (SpecialKey KeyDown) Down _ _ = modifyIORef pos (\(x,y) -> (x,y+1))
-- Default
keyboardCallback _ _ _ _ _ = return ()
