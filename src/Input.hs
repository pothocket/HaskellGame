module Input where

import Graphics.GPipe.Context.GLFW.Input

import qualified Data.Map.Strict as M

data Control = C'Up | C'Down | C'Left | C'Right deriving Eq
type ControlState = [Control]

controls :: [Control]
controls = [C'Up, C'Down, C'Left, C'Right]

getControl :: Control -> Key
getControl c = case c of
    C'Up    -> Key'W
    C'Left  -> Key'A
    C'Down  -> Key'S
    C'Right -> Key'D

isDown :: ControlState -> Control -> Bool
isDown = flip elem

