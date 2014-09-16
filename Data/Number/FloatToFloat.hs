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



{-# LANGUAGE MultiParamTypeClasses
           , OverlappingInstances
           , FlexibleInstances
           , CPP
           , MagicHash
           #-}


{-# OPTIONS_GHC -Wall -fwarn-tabs #-}



-- This module can convert any base 2 float to any other base 2 float while
-- maintaining special values such as -0.0, NaN, -Infinity, and Infinity.  It is
-- also very fast for converting between Float, CFloat, GLfloat, and Double,
-- DDouble, GLdouble.  If on GHC then conversion between these two sets is also
-- very fast.


module Data.Number.FloatToFloat(floatToFloat) where

import Foreign.C.Types(CFloat(..), CDouble(..))

#ifdef __GLASGOW_HASKELL__
import GHC.Exts(
      Float(F#)
    , Double(D#)
    , double2Float#
    , float2Double#
    )
#endif


class (RealFloat a, RealFloat b) => FloatToFloat a b where
    floatToFloat :: a -> b


-- Implementation to catch all floats (but it is slow).
-- TODO: This implementation will produce odd results if used on floats with a
--       radix other than 2.
instance (RealFloat a, RealFloat b) => FloatToFloat a b where
    floatToFloat x
        | isNaN x          = notANumber
        | isInfinite x     = if x > 0
                                then infinity
                                else negativeInfinity
        | isNegativeZero x = -0.0
        | otherwise        = uncurry encodeFloat (decodeFloat x)
        where
            notANumber       =  0.0/0.0
            infinity         =  1.0/0.0
            negativeInfinity = -1.0/0.0


instance FloatToFloat Float Float where
    {-# INLINE floatToFloat #-}
    floatToFloat = id

instance FloatToFloat CFloat CFloat where
    {-# INLINE floatToFloat #-}
    floatToFloat = id

instance FloatToFloat Double Double where
    {-# INLINE floatToFloat #-}
    floatToFloat = id

instance FloatToFloat CDouble CDouble where
    {-# INLINE floatToFloat #-}
    floatToFloat = id

instance FloatToFloat Float CFloat where
    {-# INLINE floatToFloat #-}
    floatToFloat f = CFloat f

instance FloatToFloat CFloat Float where
    {-# INLINE floatToFloat #-}
    floatToFloat (CFloat f) = f

instance FloatToFloat Double CDouble where
    {-# INLINE floatToFloat #-}
    floatToFloat d = CDouble d

instance FloatToFloat CDouble Double where
    {-# INLINE floatToFloat #-}
    floatToFloat (CDouble d) = d


#ifdef __GLASGOW_HASKELL__
instance FloatToFloat Float Double where
    {-# INLINE floatToFloat #-}
    floatToFloat (F# f) = D# (float2Double# f)

instance FloatToFloat Double Float where
    {-# INLINE floatToFloat #-}
    floatToFloat (D# d) = F# (double2Float# d)

instance FloatToFloat Float CDouble where
    {-# INLINE floatToFloat #-}
    floatToFloat (F# f) = (CDouble (D# (float2Double# f)))

instance FloatToFloat Double CFloat where
    {-# INLINE floatToFloat #-}
    floatToFloat (D# d) = CFloat (F# (double2Float# d))

instance FloatToFloat CFloat Double where
    {-# INLINE floatToFloat #-}
    floatToFloat (CFloat (F# f)) = D# (float2Double# f)

instance FloatToFloat CDouble Float where
    {-# INLINE floatToFloat #-}
    floatToFloat (CDouble (D# d)) = F# (double2Float# d)
#endif
