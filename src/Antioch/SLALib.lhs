> module Antioch.SLALib (gmst, slaGaleq) where

> import Data.Fixed    (mod')
> import Data.Function (on)
> import Data.List     (foldl1')
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

> genAngleRadPi :: Gen Double
> genAngleRadPi = choose ((-pi), pi)

> genAngleRadPiDiv2 :: Gen Double
> genAngleRadPiDiv2 = choose (((-pi)/2.0), (pi/2.0))

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

Spherical coordinates to direction cosines

> slaDcs2c :: Double -> Double -> [Double]
> slaDcs2c a b = [cos a * cosb, sin a * cosb, sin b]
>   where
>     cosb = cos b

To test slaDcs2c provide a wide range of radian inputs, and make sure that
the results are always normalized (0..1)

> inRadianRng :: Double -> Bool
> inRadianRng x = -1.0 <= x && x <= 1.0

> inPiRng :: Double -> Bool
> inPiRng x = -pi <= x && x <= pi

> almostEqual :: Double -> Double -> Bool
> almostEqual x y = if (abs (x - y)) <= 1.0e-6 then True else False

> prop_slaDcs2c =
>     forAll genAngleRad $ \a ->
>     forAll genAngleRad $ \b ->
>     let xs@([_, _ , z]) = slaDcs2c a b
>     in all inRadianRng xs && z == sin b

Equatorial to galactic rotation matrix (J2000.0), obtained by
applying the standard FK4 to FK5 transformation, for zero proper
motion in FK5, to the columns of the B1950 equatorial to
galactic rotation matrix:

> rmat = [[-0.054875539726,  0.494109453312, -0.867666135858]
>       , [-0.873437108010, -0.444829589425, -0.198076386122]
>       , [-0.483834985808,  0.746982251810,  0.455983795705]]

For testing:

> rmatInv = [[-0.054875539726, -0.873437108010, -0.483834985808]
>      , [0.494109453312, -0.444829589425, 0.746982251810]
>      , [-0.867666135858, -0.198076386122, 0.455983795705]]

Performs the 3-D backward unitary transformation:

> slaDimxv :: [Double] -> [[Double]] -> [Double]
> slaDimxv va matrix = [sum [r * a | (r, a) <- zip rs va] | rs <- matrix]

> prop_slaDimxv = forAll (choose (0, 10.0)) $ \a ->
>                 forAll (choose (0, 10.0)) $ \b ->
>                 forAll (choose (0, 10.0)) $ \c ->
>                     let [x, y, z] = slaDimxv (slaDimxv [a,b,c] rmat) rmatInv in almostEqual x a && almostEqual y b && almostEqual z c

Direction cosines to spherical coordinates (rads)

> slaDcc2s           :: [Double] -> (Double, Double)
> slaDcc2s [x, y, z] = ((if r /= 0.0 then atan2 y x else 0.0)
>                     , (if z /= 0.0 then atan2 z r else 0.0))
>          where r = sqrt(x^2 + y^2)

> prop_atan2 =
>     forAll genAngleRad $ \a ->
>     forAll genAngleRad $ \b ->
>     let x = atan2 a b in inPiRng x 

> prop_slaDcc2s =
>     forAll genAngleRadPi $ \a ->
>     forAll genAngleRadPiDiv2 $ \b ->
>     let (x, y) = slaDcc2s (slaDcs2c a b) in inPiRng x && inPiRng y && almostEqual x a && almostEqual y b 

> deg2rad deg = deg * pi / 180.0

> dsign :: Double -> Double -> Double
> dsign a b | b < 0.0   = (-(abs a))
>           | otherwise = abs a

Normalize angle into range +/- pi       
TBF: why did we have to make the above change for this to work?

> slaDrange :: Double -> Double
> slaDrange a | abs w < pi = w
>             | otherwise = w - d2pi -- dsign d2pi x
>              where w = mod' a d2pi

> prop_slaDrange = 
>     forAll genAngleRad $ \a ->
>     let x = slaDrange a in inPiRng x 

> slaGaleq dl db = ((,) `on` realToFrac) dr dd
>   where
>     (dr, dd) = (slaGaleq' `on` realToFrac) dl db
              

Transformation from IAU 1958 galactic coordinates to
  J2000.0 equatorial coordinates (double precision)
  Given:
     DL,DB       galactic longitude and latitude L2,B2
  Returned:
     DR,DD       J2000.0 RA,Dec
  (all arguments are radians)              

> slaGaleq' :: Double -> Double -> (Double, Double)
> slaGaleq' dl db = (dranrm dr, slaDrange dd)
>     where (dr, dd) = slaDcc2s (slaDimxv (slaDcs2c dl db) rmat)

> prop_slaGaleq = 
>     forAll genGalacticLongitude $ \a ->
>     forAll genGalacticLatitude $ \b ->
>     let (x, y) = slaGaleq a b in validRA x && validDec y  

Galactic coords are Longitude 0-360 degrees, and Lat. -90 - 90 degrees.

> genGalacticLongitude :: Gen Double
> genGalacticLongitude = choose (0.0, d2pi)

> genGalacticLatitude :: Gen Double
> genGalacticLatitude = choose ((-pi/2.0), pi/2.0)

RA & Dec: 0-24 hrs, -90 - 90 degrees

> validRA :: Double -> Bool
> validRA x = 0.0 <= x && x <= d2pi --24.0

> validDec :: Double -> Bool
> validDec x = (-pi/2.0) <= x && x <= pi/2.0 

