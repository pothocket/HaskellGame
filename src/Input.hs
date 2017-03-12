{-# LANGUAGE TypeFamilies #-}
module Input where

import Prelude hiding ((.))

import Graphics.GPipe.Context.GLFW.Input
import Graphics.GPipe.Context.GLFW (GLFWWindow)
import Graphics.GPipe.Context (ContextT)

import Control.Wire

import qualified Data.Map.Strict as M

data Control = C'Up | C'Down | C'Left | C'Right deriving Eq
type ControlState = [Control]

type KeyEvent = Event Control

controls :: [Control]
controls = [C'Up, C'Down, C'Left, C'Right]

getControl :: Control -> Key
getControl c = case c of
    C'Up    -> Key'W
    C'Left  -> Key'A
    C'Down  -> Key'S
    C'Right -> Key'D

getControlState :: ContextT GLFWWindow os f IO ControlState
getControlState = do
    let states :: ContextT GLFWWindow os f IO [KeyState]
        states = mapM (getKey . getControl) controls
    let pairs :: ContextT GLFWWindow os f IO [(Control, KeyState)]
        pairs = zip <$> pure controls <*> states
    fmap (map fst . filter((==KeyState'Pressed) . snd)) pairs

isDown :: ControlState -> Control -> Bool
isDown = flip elem

isKeyDown :: Control -> Wire s () (ContextT GLFWWindow os f IO) a ()
isKeyDown c = mkGen_ $ \_ -> do
    controlState <- getControlState
    return $ if isDown controlState c 
             then Right ()
             else Left ()