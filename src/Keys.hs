module Keys where

import GameState

import Graphics.UI.GLUT
import System.Exit (exitSuccess)
import Data.IORef

keyboardCallback :: IORef GameState -> Key -> KeyState -> Modifiers -> Position -> IO()
-- Exit keys
keyboardCallback _ (Char '\ESC') Down _ _ = exitSuccess
-- Movement keys
keyboardCallback gs (SpecialKey KeyRight) Down _ _ =
	modifyIORef gs (\g@GameState {hero = h} -> g {hero = h {dir = DirRight}})
keyboardCallback gs (SpecialKey KeyLeft) Down _ _ =
	modifyIORef gs (\g@GameState {hero = h} -> g {hero = h {dir = DirLeft}})
keyboardCallback gs (SpecialKey KeyUp) Down _ _ =
	modifyIORef gs (\g@GameState {hero = h} -> g {hero = h {dir = DirUp}})
keyboardCallback gs (SpecialKey KeyDown) Down _ _ =
	modifyIORef gs (\g@GameState {hero = h} -> g {hero = h {dir = DirDown}})
-- Stop key - temporarily
keyboardCallback gs (Char ' ') Down _ _ =
	modifyIORef gs (\g@GameState {hero = h} -> g {hero = h {dir = DirIdle}})
-- Default
keyboardCallback _ _ _ _ _ = return ()
