{-# LANGUAGE TupleSections, TemplateHaskell #-}

module Game where

import Linear
import Graphics.GPipe.PrimitiveArray
import Control.Lens

import Util
import Constants
import Input

-------------------------------------------------------------------------

data World = World { _player    :: Player
                   , _entities :: [Entity]
                   , _timeMS   :: Int}


data Entity = Box {_eInfo :: EntityInfo}
type Player = Entity

data EntityInfo = EntityInfo { _eModel :: EntityModel
                             , _ePos :: V2 Float
                             , _eVel :: V2 Float }

type EntityModel = (PrimitiveTopology Triangles, [(V2 Float, V3 Float)])

makeLenses ''World
makeLenses ''Entity
makeLenses ''EntityInfo

-------------------------------------------------------------------------
--World Code--

initWorld :: World
initWorld = World newBox [] 0

updateWorld :: ControlState -> Int -> World -> World
updateWorld keys dt = foldl1 (.) [ processInput keys
                                 , player %~ updatePos dt
                                 , entities %~ fmap (updatePos dt)
                                 , timeMS +~ dt
                                 ]

processInput :: ControlState -> World -> World
processInput = flip const

-------------------------------------------------------------------------
--Entity Code--

updatePos :: Int -> Entity -> Entity
updatePos dt ent = (eInfo . ePos) +~ (ent ^. eInfo . eVel * (fromIntegral dt) / (1000/fps)) $ ent

setVel :: V2 Float -> Entity -> Entity
setVel v = eInfo . eVel .~ v

--Player Code--


-------------------------------------------------------------------------
--Models--

boxModel :: EntityModel
boxModel = (TriangleStrip, 
            map (,red) [ V2 0 0
                       , V2 0 64
                       , V2 64 0
                       , V2 64 64 ]
           ) where red = V3 1 0 0

idkModel :: EntityModel
idkModel = (TriangleStrip, 
            map (,col) [ V2 0 0
                       , V2 32 0
                       , V2 64 96
                       , V2 0 64 ]
           ) where col = V3 1 1 0


newBox :: Entity
newBox = Box $ EntityInfo boxModel (V2 0 0) (V2 0 0)

