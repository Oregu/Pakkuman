module Keys where

import GameState

import Graphics.UI.GLUT
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Data.IORef

keyboardCallback :: IORef GameState -> Key -> KeyState -> Modifiers -> Position -> IO()
-- Exit keys
keyboardCallback _ (Char '\ESC') Down _ _ = exitWith ExitSuccess
-- Movement keys
keyboardCallback gs (SpecialKey KeyRight) Down _ _ = modifyIORef gs (\(GameState {hero = Hero {pos = (x,y)}}) -> GameState {hero = Hero {pos = (x+1, y)}})
keyboardCallback gs (SpecialKey KeyLeft) Down _ _ = modifyIORef gs (\(GameState {hero = Hero {pos = (x,y)}}) -> GameState {hero = Hero {pos = (x-1, y)}})
keyboardCallback gs (SpecialKey KeyUp) Down _ _ = modifyIORef gs (\(GameState {hero = Hero {pos = (x,y)}}) -> GameState {hero = Hero {pos = (x, y-1)}})
keyboardCallback gs (SpecialKey KeyDown) Down _ _ = modifyIORef gs (\(GameState {hero = Hero {pos = (x,y)}}) -> GameState {hero = Hero {pos = (x, y+1)}})
-- Default
keyboardCallback _ _ _ _ _ = return ()
