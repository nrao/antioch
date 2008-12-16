> module Antioch.SLALib (gmst, gmst') where

> import Data.Fixed  (mod')
> import Data.List   (foldl1')

> d2pi :: Double
> d2pi = 2.0 * pi

> s2r :: Double
> s2r = 7.272205216643039903848711535369e-5

> -- Normalize angle into the range 0..2*pi.
> dranrm a
>     | a <  0    = a +      d2pi
>     | otherwise = a `mod'` d2pi

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

> gmst' :: Float -> Float
> gmst' = realToFrac . gmst . realToFrac

> -- Conversion from universal time to Greenwich mean sidereal time.
> -- ut1 is modified julian date (jd - 2400000.5)
> gmst ut1 =
>     (ut1 `mod'` 1.0) * d2pi + s2r * poly tu [-6.2e-6, 0.093104, 8640184.812866, 24110.54841]
>   where
>     tu = (ut1 - 51544.5) / 36525.0

> poly x = foldl1' $ \a b -> a*x + b
