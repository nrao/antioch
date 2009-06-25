> module Server.List where

> lookup'          :: Eq a => Maybe b -> a -> [(a, Maybe b)] -> Maybe b
> lookup' def name = maybe def id . lookup name

Break a list `xs` into groups of no more than `n` elements.

> partition' _ [] = []
> partition' n xs = x : partition' n xs'
>   where
>     (x, xs') = splitAt n xs

> split _ []      = [[]]
> split d (c:cs)
>     | d == c    = []    : rs
>     | otherwise = (c:r) : rs'
>   where
>     rs@(r:rs') = split d cs
