multiply :: Int -> Int -> Int
multiply a 1 = a
multiply a 0 = 0

multiply a b
    | b > 0     = a + multiply a (b - 1)
    | b < 0     = -multiply a (-b)