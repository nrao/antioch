> module Antioch.SLALib (gmst, slaGaleq) where

> import Data.Fixed  (mod')
> import Data.List   (foldl1')
> import Test.QuickCheck 

> d2pi :: Double
> d2pi = 2.0 * pi

> s2r :: Double
> s2r = 7.272205216643039903848711535369e-5

> -- Normalize angle into the range 0..2*pi.
> dranrm a
>     | a <  0    = dranrm $ a + d2pi
>     | otherwise = a `mod'` d2pi

Even though this one is fairly obvious, lets start unit testing;
Generate a wide range of angles:

> genAngleRad :: Gen Double
> genAngleRad = choose (10*(-d2pi), 10*d2pi)

Now make sure that 'dranrm' always normalizes them:

> prop_dranrm = forAll genAngleRad $
>     \a -> let b = dranrm a in 0.0 <= b && b <= d2pi

> floatConv f = realToFrac . f . realToFrac

The IAU 1982 expression (see page S15 of 1984 Astronomical Almanac) is
used, but rearranged to reduce rounding errors.  This expression is
always described as giving the GMST at 0 hours UT.  In fact, it gives
the difference between the GMST and the UT, which happens to equal the
GMST (modulo 24 hours) at 0 hours UT each day.  In this routine, the
entire UT is used directly as the argument for the standard formula,
and the fractional part of the UT is added separately.  Note that the
factor 1.0027379... does not appear in the IAU 1982 expression
explicitly but in the form of the coefficient 8640184.812866, which is
86400x36525x0.0027379...

> gmst :: Float -> Float
> gmst = floatConv gmst'

> -- Conversion from universal time to Greenwich mean sidereal time.
> -- ut1 is modified julian date (jd - 2400000.5)
> gmst' ut1 =
>     (ut1 `mod'` 1.0) * d2pi + s2r * poly tu [-6.2e-6, 0.093104, 8640184.812866, 24110.54841]
>   where
>     tu = (ut1 - 51544.5) / 36525.0

> poly x = foldl1' $ \a b -> a*x + b

Let's test the 'ut1' function: first, generate UT inputs:
(TBD: what are the bounds?):

> --genUTTime :: Gen Double
> --genUTTime = choose (0.0, 2400000.5)

Now make sure that the gmst time makes sense (TBD: what are the bounds?):

> -- prop_gmst' = forAll genUTTime $ \a -> let b = gmst' a in 0.0 <= b && b <= 2400000.5

Transformation from IAU 1958 Galactic coordinates to J2000.0 equatorial coordinates.

 Copyright P.T.Wallace.  All rights reserved.

> slaDcs2c :: Double -> Double -> [Double]
> slaDcs2c a b = [cos a * cosb, sin a * cosb, sin b]
>   where
>     cosb = cos b

To test slaDcs2c provide a wide range of radian inputs, and make sure that
the results are always normalized (0..1)
TBF: multiple args?

> -- prop_slaDcs2c =  forAll genAngleRad $ \a b let x, y = slaDcs2c

> rmat = [[-0.054875539726,  0.494109453312, -0.867666135858]
>       , [-0.873437108010, -0.444829589425, -0.198076386122]
>       , [-0.483834985808,  0.746982251810,  0.455983795705]]

> slaDimxv :: [Double] -> [Double]
> slaDimxv va = [sum [r * a | r <- rs,  a <- va] | rs <- rmat]

> slaDcc2s           :: [Double] -> (Double, Double)
> slaDcc2s [x, y, z] = ((if r /= 0.0 then atan2 y x else 0.0)
>                     , (if z /= 0.0 then atan2 z r else 0.0))
>          where r = sqrt(x^2 + y^2)

> deg2rad deg = deg * pi / 180.0

> dsign :: Double -> Double -> Double
> dsign a b | b < 0.0   = (-a)
>           | otherwise = a
      
> slaDrange :: Double -> Double
> slaDrange a | abs w < pi = w
>             | otherwise  = w - dsign d2pi a
>             where w = mod' a d2pi

> slaGaleq dl db = (realToFrac dr, realToFrac dd)
>   where
>     (dr, dd) = slaGaleq' (realToFrac dl) (realToFrac db)
              
> slaGaleq' :: Double -> Double -> (Double, Double)
> slaGaleq' dl db = (dranrm dr, slaDrange dd)
>     where (dr, dd) = slaDcc2s (slaDimxv (slaDcs2c dl db))
