{-# LANGUAGE TypeFamilies #-}
module Input (Control(..), keyPressed, keyReleased, whileKeyPressed) where

import Prelude hiding ((.), id)

import Graphics.GPipe.Context.GLFW.Input
import Graphics.GPipe.Context.GLFW (GLFWWindow)
import Graphics.GPipe.Context (ContextT)

import Control.Wire 

import qualified Data.Map.Strict as M

data Control = C'Up | C'Down | C'Left | C'Right deriving Eq
type ControlState = [Control]

type KeyEvent = Event ControlState

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

isDown :: Control -> ControlState -> Bool
isDown = elem

keyStateChanged :: (m ~ ContextT GLFWWindow os f IO) => (Bool -> Bool) -> Control -> Wire s () m a KeyEvent
keyStateChanged p control = eventWire . keyWire
    where eventWire :: Wire s e m ControlState KeyEvent
          eventWire = became (p <$> isDown control)
          keyWire :: Wire s e (ContextT GLFWWindow os f IO) a ControlState
          keyWire = mkGen_ $ \_ -> do
              cs <- getControlState
              return $ Right cs

keyPressed = keyStateChanged id
keyReleased = keyStateChanged not

whileKeyPressed :: Control -> Wire s () (ContextT GLFWWindow os f IO) a () 
whileKeyPressed c = between . liftA3 (,,) (pure ()) (keyPressed c) (keyReleased c)