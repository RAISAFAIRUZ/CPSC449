powerOfTwoMaybe :: Integer -> Maybe Integer
powerOfTwoMaybe a
    | a > 0    = Just (2 ^ a)
    | a == 0    = Just 1
    | otherwise = Nothing


data SF a = SS a | FF deriving (Show)

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x-1)

{-}
showFunction :: SF Integer -> String
showFunction FF = "FF"
showFunction (SS b) = "SS " ++ show b -}

factorialSF :: Integer -> SF Integer
factorialSF a 
    | a > 0 = SS (factorial a)
    | a == 0 = SS 1
    | otherwise = FF 

   

invExpEither :: Integer -> Either String Integer
invExpEither x
    | x < 0      = Left "Input must be a non-negative integer"  
    | otherwise  = Right (findLargest 0 1)                   
  where
    findLargest a b
        | b > x = a - 1              
        | otherwise = findLargest (a + 1) (b * 2)  


 {- Citation: 
 https://wiki.haskell.org/Let_vs._Where
 https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving.html
    https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/deriving.html-}

