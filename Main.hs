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
import Data.Packed.Vector
import Data.Packed.Matrix
import Numeric.GSL.ODE
import Control.Monad


-- The Lorenz Attractor program.
main :: IO ()
main = mapM_ (printStuff) a
    where
        a = toLists(odeSolve (lorenz' 10 28 2.6) [1,1,1] (fromList [0,0.01..1000]))

printStuff :: [Double] -> IO ()
printStuff (x:y:z:_) = putStrLn ((show x) ++ "  " ++ (show y) ++ "  " ++ (show z))

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



-- -- Font to use in glutPrint.
-- font :: BitmapFont
-- font = Helvetica18
--
--
-- -- GLUT string printer.
-- glutPrint :: String -> IO ()
-- glutPrint = renderString font 
--
--
-- -- Print OpenGL errors.
-- printErrors :: IO ()
-- printErrors = get errors >>= mapM_ (hPutStrLn stderr . show)
--
