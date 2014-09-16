{-------------------------------------------------------------------------------
 -  Copyright (c) 2014 Michael R. Shannon
 -
 -  Permission is hereby granted, free of charge, to any person obtaining
 -  a copy of this software and associated documentation files (the
 -  "Software"), to deal in the Software without restriction, including
 -  without limitation the rights to use, copy, modify, merge, publish,
 -  distribute, sublicense, and/or sell copies of the Software, and to
 -  permit persons to whom the Software is furnished to do so, subject to
 -  the following conditions:
 -
 -  The above copyright notice and this permission notice shall be included
 -  in all copies or substantial portions of the Software.
 -
 -  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 -  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 -  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 -  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 -  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 -  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 -  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 ------------------------------------------------------------------------------}


module Main where

import Graphics.UI.GLUT
import Data.IORef
import System.IO
import qualified Data.Packed.Vector as V
import qualified Data.Packed.Matrix as M
import Numeric.GSL.ODE
import Control.Monad

import Paths_lorenz(version)
import Data.Version(showVersion)








-- Amount to scale unit cube orthogonal projection.
viewScale = 3


-- The Lorenz Attractor program.
main :: IO ()
main = do
        initilizeGLUT
        mainLoop



initilizeGLUT :: IO ()
initilizeGLUT = do
        -- Initilize and open window.
        (_, _) <- getArgsAndInitialize
        initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
        _ <- createWindow windowTitle
        -- Callbacks.
        specialCallback $= Nothing
        reshapeCallback $= Just reshape
        idleCallback    $= Nothing
        displayCallback $= display
        -- OpenGL settings.
        depthFunc       $= Just Lequal
    where
        windowTitle = "Lorenz Atractor (v" ++ showVersion version ++ ")"


-- This function is called by GLUT to display the scene.
display :: DisplayCallback
display = do
    -- Clear and reset.
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    -- Draw RGB triangle.
    renderPrimitive Triangles $ do
        color  $ (Color3    1.0    0.0   0.0 :: Color3 GLfloat)
        vertex $ (Vertex2   0.0    0.5       :: Vertex2 GLfloat)
        color  $ (Color3    0.0    1.0   0.0 :: Color3 GLfloat)
        vertex $ (Vertex2   0.5  (-0.5)      :: Vertex2 GLfloat)
        color  $ (Color3    0.0    0.0   1.0 :: Color3 GLfloat)
        vertex $ (Vertex2 (-0.5) (-0.5)      :: Vertex2 GLfloat)
    -- Draw and swap buffers.
    flush
    swapBuffers
    -- Check for errors.
    printErrors




-- This funtion is called by GLUT when the window is resized.
reshape :: ReshapeCallback
reshape (Size width height) = do
        -- Set viewport as entire window.
        viewport $= (Position 0 0, Size width height)
        -- Orthogonal projection: scaled cube adjusted for aspect ratio.
        matrixMode $= Projection
        loadIdentity
        ortho (-w2h*viewScale) (w2h*viewScale)  -- left right
              (-1.0*viewScale) (1.0*viewScale)  -- bottom top
              (-1.0*viewScale) (1.0*viewScale)  -- near far
        -- Reset model view to the identity matrix.
        matrixMode $= Modelview 0
        loadIdentity
    where
        -- Width to height ratio.
        w2h = if height > 0
                then (fromIntegral width)/(fromIntegral height)
                else 1




-- Lorenz attractor function.
-- σ -> ρ -> β -> t -> (x, y, z) -> (dxdt, dydt, dzdt)
lorenz :: Num a => a -> a -> a -> a -> (a, a, a) -> (a, a, a)
lorenz s r b t (x, y, z) = (dxdt, dydt, dzdt)
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
