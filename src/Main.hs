{-# LANGUAGE RankNTypes, ScopedTypeVariables, PackageImports, TypeFamilies, TupleSections #-}

module Main where

import Graphics.GPipe
import Graphics.GPipe.Context.GLFW (newContext', GLFWWindow, WindowConf)
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import System.IO (hSetBuffering, BufferMode(..), stdout)

import Control.Arrow ((>>>))
import Control.Lens

import Constants
import Shader
import Game
import Body

import Util

theWindow :: ContextFactory c ds GLFWWindow
theWindow = newContext' [] (GLFW.WindowConf gWidth gHeight gTitle)

main :: IO ()
main = runContextT theWindow (ContextFormatColor RGB8) $ do
    vertexBuffer :: Buffer os (B2 Float, B3 Float) <- newBuffer 4
    posBuffer :: Buffer os (B2 Float) <- newBuffer 1
    writeBuffer vertexBuffer 0 $ map (,(V3 1 1 1)) (map ((+(V2 1 (0))) . toGLPos) myTriangle)
    writeBuffer posBuffer 0 $ fmap toGLPos [V2 100 250]
    lift $ hSetBuffering stdout NoBuffering
    
    shader <- compileShader myShader
    loop vertexBuffer posBuffer initWorld shader

loop :: (Num a, Color c Float ~ V3 a, ContextColorFormat c, b2 ~ (b,b1),
         HostFormat b ~ (V2 Float, V3 Float), HostFormat b1 ~ V2 Float)
     => Buffer os b
     -> Buffer os b1
     -> World
     -> CompiledShader os (ContextFormat c ds) (PrimitiveArray Triangles b2)
     -> ContextT GLFWWindow os (ContextFormat c ds) IO ()
loop vb pb world shader = do
    let ent = view (entities . to head) world
    time1 <- liftIO getTimeMS
    render $ do
        clearContextColor (V3 0 0 0)
        posArray <- newVertexArray pb
        vertexArray <- newVertexArray vb
        let primitiveArray = toPrimitiveArrayInstanced (ent ^. eInfo . eModel . to fst) (,) vertexArray posArray
        shader primitiveArray
    swapContextBuffers

    writeBuffer vb 0 (ent ^. eInfo . eModel . to snd)
    writeBuffer pb 0 [ent ^. eInfo . ePos]

    time2 <- liftIO getTimeMS
    let dt = time2 - time1
        world' = foldr1 (.) [ updateWorld dt
                              , timeMS +~ dt
                              ] world

    closeRequested <- GLFW.windowShouldClose
    unless closeRequested $
        loop vb pb world' shader


myShader :: Shader os (ContextFormat RGBFloat ds) (PrimitiveArray p ((B2 Float, B3 Float), B2 Float)) ()
myShader = do
    ps <- toPrimitiveStream id
    let ps' = fmap transformStream ps
    fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 gWidth gHeight), DepthRange 0 1)) ps'
    drawContextColor (const (ContextColorOption NoBlending (V3 True True True))) fragmentStream


myTriangle :: [V2 Float]
myTriangle = [ V2 0 0
             , V2 64 0
             , V2 0 64
             ]

toV4Pos :: (Num a) => V2 a -> V4 a
toV4Pos (V2 x y) = V4 x y 0 1
