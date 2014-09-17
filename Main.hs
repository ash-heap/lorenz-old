{-------------------------------------------------------------------------------
 --  Copyright (c) 2014 Michael R. Shannon
 --
 --  Permission is hereby granted, free of charge, to any person obtaining
 --  a copy of this software and associated documentation files (the
 --  "Software"), to deal in the Software without restriction, including
 --  without limitation the rights to use, copy, modify, merge, publish,
 --  distribute, sublicense, and/or sell copies of the Software, and to
 --  permit persons to whom the Software is furnished to do so, subject to
 --  the following conditions:
 --
 --  The above copyright notice and this permission notice shall be included
 --  in all copies or substantial portions of the Software.
 --
 --  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 --  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 --  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 --  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 --  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 --  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 --  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 ------------------------------------------------------------------------------}

{-# OPTIONS_GHC -O2 -Wall -Werror #-}

module Main where

import Graphics.UI.GLUT
import Data.IORef
import System.IO

import Paths_lorenz(version)
import Data.Version(showVersion)


-- Convenience types.
type Vertex3f = Vertex3 GLfloat
type Color3f  = Color3 GLfloat
type Vector3f = Vector3 GLfloat


-- Helpful base vectors.
zeroVector3f :: Vector3f
zeroVector3f = Vector3 0.0 0.0 0.0
xVector3f :: Vector3f
xVector3f = Vector3 1.0 0.0 0.0
yVector3f :: Vector3f
yVector3f = Vector3 0.0 1.0 0.0
zVector3f :: Vector3f
zVector3f = Vector3 0.0 0.0 1.0

-- Helpful base vectors.
originVertex3f :: Vertex3f
originVertex3f = Vertex3 0.0 0.0 0.0
xVertex3f :: Vertex3f
xVertex3f = Vertex3 1.0 0.0 0.0
yVertex3f :: Vertex3f
yVertex3f = Vertex3 0.0 1.0 0.0
zVertex3f :: Vertex3f
zVertex3f = Vertex3 0.0 0.0 1.0

-- Some base colors.
black :: Color3f
black = Color3 0.0 0.0 0.0 :: Color3f
white :: Color3f
white = Color3 1.0 1.0 1.0 :: Color3f
red :: Color3f
red = Color3 1.0 0.0 0.0 :: Color3f
green :: Color3f
green = Color3 0.0 1.0 0.0 :: Color3f
blue :: Color3f
blue = Color3 0.0 0.0 1.0 :: Color3f
cyan :: Color3f
cyan = Color3 0.0 1.0 1.0 :: Color3f
magenta :: Color3f
magenta = Color3 1.0 0.0 1.0 :: Color3f
yellow :: Color3f
yellow = Color3 1.0 1.0 0.0 :: Color3f



vertexToVector3 :: Vertex3 a -> Vector3 a
vertexToVector3 (Vertex3 x y z) = (Vector3 x y z)


vectorToVertex3 :: Vector3 a -> Vertex3 a
vectorToVertex3 (Vector3 x y z) = (Vertex3 x y z)



-- Contains the values related to the view.
data View = View { viewScale :: GLfloat -- scale
                 , viewCenter          :: Vertex3f -- center of projection
                 , viewAzimuth         :: GLfloat -- rotation
                 , viewElevation       :: GLfloat -- rotation
                 } deriving (Show)


data App = App { appView :: View } deriving (Show)



-- -- This represents a 3D parametric function.
-- data Function = Function { time   :: [GLfloat]
--                          , points :: [Vertex3f]
--                          } deriving (Show)



initialView :: View
initialView = View 
    10.0 -- scale
    (Vertex3 0.0 0.0 0.0) -- center
    (-20.0) -- azimuth
    20.0 -- elevation



initialApp :: App
initialApp = App initialView




-- The Lorenz Attractor program.
main :: IO ()
main = do
        appRef <- newIORef initialApp
        initilizeGLUT appRef
        mainLoop



initilizeGLUT :: IORef App -> IO ()
initilizeGLUT appRef = do
        -- Initilize and open window.
        (_, _) <- getArgsAndInitialize
        initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
        _ <- createWindow title
        -- Callbacks.
        specialCallback $= Nothing
        reshapeCallback $= Just reshape
        idleCallback    $= Nothing
        displayCallback $= display appRef
        -- OpenGL settings.
        depthFunc       $= Just Lequal
    where
        title = "Lorenz Atractor (v" ++ showVersion version ++ ")"




-- This function is called by GLUT to display the scene.
display :: IORef App -> DisplayCallback
display appRef = do
        app <- readIORef appRef
        -- Clear and set the view.
        clear [ColorBuffer, DepthBuffer]
        applyView . appView $ app
        -- Draw axis.
        preservingMatrix $ do
            (\s -> scale s s s) . viewScale . appView $ app
            drawAxes
        -- Draw and swap buffers.
        flush
        swapBuffers
        -- Check for errors.
        printErrors




applyView :: View -> IO ()
applyView (View scaleFactor center azimuth elevation) = do
        loadIdentity
        scale s s s
        translate $ fmap negate (vertexToVector3 center)
        rotate elevation xVector3f
        rotate azimuth yVector3f
        rotate (-90) zVector3f
        rotate (-90) yVector3f
    where
        s = 1.0/scaleFactor



applyProjection :: Size -> IO ()
applyProjection (Size width height) = do
        -- Orthogonal projection: unit cube adjusted for aspect ratio.
        matrixMode $= Projection
        loadIdentity
        if w2h >= 1
            -- Wider than taller.
            then ortho (-w2h) (w2h) (-1.0)   (1.0)   (-1.0) (1.0)
            -- Taller than wider.
            else ortho (-1.0) (1.0) (-1/w2h) (1/w2h) (-1.0) (1.0)
    where
        -- Width to height ratio.
        w2h = if height > 0
                then (fromIntegral width)/(fromIntegral height)
                else 1


-- This funtion is called by GLUT when the window is resized.
reshape :: ReshapeCallback
reshape size = do
        -- Set viewport as entire window.
        viewport $= (Position 0 0, size)
        -- Apply the projection.
        applyProjection size
        -- Reset the modelview matrix.
        matrixMode $= Modelview 0
        loadIdentity



drawAxes :: IO ()
drawAxes = preservingMatrix $ do
        scale s s s
        -- Axes lines.
        drawVector red   xVector3f
        drawVector green yVector3f
        drawVector blue  zVector3f
        -- Draw point at origin.
        drawPoint 5.0 white originVertex3f
    where
        s = 0.7 :: GLfloat



drawPoint :: GLfloat -> Color3f -> Vertex3f -> IO ()
drawPoint p c v = do
        pointSize $= p
        renderPrimitive Points $ do
            color c
            vertex v


drawVector :: Color3f -> Vector3f -> IO ()
drawVector c (Vector3 x y z) =
        renderPrimitive Lines $ do
            color c
            vertex originVertex3f
            vertex $ Vertex3 x y z







-- Lorenz attractor function.
-- σ -> ρ -> β -> t -> (x, y, z) -> (dxdt, dydt, dzdt)
lorenz :: Num a => a -> a -> a -> a -> (a, a, a) -> (a, a, a)
lorenz s r b _ (x, y, z) = (dxdt, dydt, dzdt)
    where
        dxdt = s * (y - x)
        dydt = x * (r - z) - y
        dzdt = x * y - b * z





-- Same as the lorenz attractor function but uses lists instead of tuples and
-- thus can have a runtime error.
-- σ -> ρ -> β -> t -> [x, y, z] -> [dxdt, dydt, dzdt]
lorenz' :: Num a => a -> a -> a -> a -> [a] -> [a]
lorenz' s r b t (x:y:z:_) = [dxdt, dydt, dzdt]
    where 
        (dxdt, dydt, dzdt) = lorenz s r b t (x, y, z)
lorenz' _ _ _ _ _ = error "lorenz' needs a list with exactly 3 elements"



-- Font to use in glutPrint.
font :: BitmapFont
font = Helvetica18


-- GLUT string printer.
glutPrint :: String -> IO ()
glutPrint = renderString font 


-- Print OpenGL errors.
printErrors :: IO ()
printErrors = get errors >>= mapM_ (hPutStrLn stderr . show)
--
