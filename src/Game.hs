{-# LANGUAGE TupleSections, TemplateHaskell #-}

module Game where

import Linear
import Graphics.GPipe.PrimitiveArray
import Control.Lens

import Util
import Constants

-------------------------------------------------------------------------

data World = World { _entities :: [Entity]
                   , _timeMS :: Int}

data Entity = Box {_eInfo :: EntityInfo}

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
initWorld = World [newBox] 0

updateWorld :: Int -> World -> World
updateWorld dt = (entities %~ fmap (updatePos dt))

-------------------------------------------------------------------------
--Entity Code--

updatePos :: Int -> Entity -> Entity
updatePos dt ent = (eInfo . ePos) +~ (ent ^. eInfo . eVel * (fromIntegral dt) / (1000/fps)) $ ent

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
newBox = Box $ EntityInfo boxModel (V2 0 0) (V2 1 0)

