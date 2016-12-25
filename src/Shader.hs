module Shader where

import Linear
import Constants

transformStream :: (Floating a) 
                => ((V2 a, V3 a), V2 a)
                -> (V4 a, V3 a)
transformStream ((pos, c), V2 xPos yPos) = (V4 xFinal yFinal 0 1, c)
    where V2 xFinal yFinal = (toGLPos (pos + V2 xPos yPos))

toGLPos :: (Floating a) => V2 a -> V2 a
toGLPos (V2 x y) = V2 ((x/fWidth - 0.5)*2) ((0.5 - y/fHeight)*2)

