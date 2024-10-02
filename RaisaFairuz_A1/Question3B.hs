
myShow :: Int -> String
myShow x 
    | x == 0 = ['0']
    | x == 1 = ['1']
    | x == 2 = ['2']
    | x == 3 = ['3']
    | x == 4 = ['4']
    | x == 5 = ['5']
    | x == 6 = ['6']
    | x == 7 = ['7']
    | x == 8 = ['8']
    | x == 9 = ['9']
    | x > 9 = myShow (x `div` 10)  ++ myShow(mod x 10 ) 

myLength :: String -> Int
myLength [] = 0                     
myLength (first:rest) = 1 + myLength rest 

myEven :: Int -> Bool
myEven a 
    | a `mod` 2 == 0 = True
    | otherwise      = False

processAndCheck :: Int -> Bool
processAndCheck = myEven . myLength . myShow

{- Citation : 
https://wiki.haskell.org/Function_composition
https://stackoverflow.com/questions/36291482/what-is-the-xs-notation-in-haskell
-}
