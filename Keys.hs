module Keys where

import GameState

import Graphics.UI.GLUT
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Data.IORef

keyboardCallback :: IORef GameState -> Key -> KeyState -> Modifiers -> Position -> IO()
-- Exit keys
keyboardCallback _ (Char '\ESC') Down _ _ = exitWith ExitSuccess
-- Movement keys
keyboardCallback gs (SpecialKey KeyRight) Down _ _ =
	modifyIORef gs (\(GameState {hero = Hero {pos = p, dir = _}, ghosts = g}) -> GameState {hero = Hero {pos = p, dir = DirRight}, ghosts = g})
keyboardCallback gs (SpecialKey KeyLeft) Down _ _ =
	modifyIORef gs (\(GameState {hero = Hero {pos = p, dir = _}, ghosts = g}) -> GameState {hero = Hero {pos = p, dir = DirLeft}, ghosts = g})
keyboardCallback gs (SpecialKey KeyUp) Down _ _ =
	modifyIORef gs (\(GameState {hero = Hero {pos = p, dir = _}, ghosts = g}) -> GameState {hero = Hero {pos = p, dir = DirUp}, ghosts = g})
keyboardCallback gs (SpecialKey KeyDown) Down _ _ =
	modifyIORef gs (\(GameState {hero = Hero {pos = p, dir = _}, ghosts = g}) -> GameState {hero = Hero {pos = p, dir = DirDown}, ghosts = g})
-- Stop key - temporarily
keyboardCallback gs (Char ' ') Down _ _ =
	modifyIORef gs (\(GameState {hero = Hero {pos = p, dir = _}, ghosts = g}) -> GameState {hero = Hero {pos = p, dir = DirIdle}, ghosts = g})
-- Default
keyboardCallback _ _ _ _ _ = return ()
