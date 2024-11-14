import Distribution.Compat.Graph (nodeValue)
-- Question 1

processNestedList :: [[Int]] -> [Int]

-- the inner list is flattened with concat then only the even numbers are filtered which then gets squared
processNestedList innerList = map (^2) (filter even (concat innerList))

fst' :: (a, b) -> a
-- uncurried form takes a tupple. returns the first element x from the two pair tuple 
fst' = uncurry (\x y -> x)

snd' :: (a, b) -> b
--uncurried form takes a tuple as input and returns an integer. snd' returns the second element y of the two pair tuple 
snd' = uncurry (\ x y -> y)

-- Question 2
matrixAddition :: [[Int]] -> [[Int]] -> [[Int]]
--Outer zipWith applies zipWith (+) to both the input lists, meaning the eleements that are at the same position in the lists gets added
-- learned zipwith in detail from this website http://www.zvon.org/other/haskell/Outputprelude/zipWith_f.html
matrixAddition = zipWith (zipWith (+))

scalarMultiply :: Int -> [[Int]] -> [[Int]]
scalarMultiply scalarValue = map (map (* scalarValue))
-- this is a partial application where a variable is passed to make a new function. this is a curried funtion because the input is a sequential function. 
--let doubleMatrix = scalarMultiply 2 doubleMatrix takes a matrux and multiplies it by the scalar 2 after defining it previously

matrixTrace :: [[Int]] -> Int
-- zipWith (!!) pair each row with its corresponding index and takes out the element that's i=j (since square matrix), so in diagonal position. 
--starting from 0, foldr (+) just adds all the elements in the list from zipWIth
matrixTrace inputMatrix = foldr (+) 0 (zipWith (!!) inputMatrix [0..])

-- Question 3
-- given starter code
class Shape a where
    area :: a -> Double
data Circle = Circle Double
data Rectangle = Rectangle Double Double

-- here circle is an instance of the class 'Shape' and it makes use of the area function which will be the method to calculate the area of circle
instance Shape Circle where
    area (Circle radius) = pi * radius ^ 2

-- here Rectangle is an instance of the class 'Shape' and it makes use of the area function which will be the method to calculate the area of the rectangle
instance Shape Rectangle where
    area (Rectangle length width) = length * width

-- Question 4
data AVLTree a = Empty | Node a (AVLTree a) (AVLTree a)
                deriving (Show, Eq) -- allows the output of the tree

heightAVL :: AVLTree a -> Int
-- height of an empty tree is -1
heightAVL Empty = -1

-- for any particular node the height is 1 plus the maximum height of its left or right subtree
heightAVL (Node nodeValue left right ) = 1 + max (heightAVL left) (heightAVL right)

isOrdered :: (Ord a) => AVLTree a -> Bool

-- helper functions isCorrectOrder and traverseTree
-- traverseTree goes through each element/ node of the tree and turn the whole tree into a list. left - node - right
-- isCorrectOrder then takes in that list and checks if it is in order
isOrdered avlTree = isCorrectOrder (traverseTree avlTree)
    where
        -- takes in the tree by node and returns a list
        traverseTree :: AVLTree a -> [a]
        traverseTree Empty = []
        -- empty tree cannot be traversed so returns an empty list
        --the left subtree's values are traversed first, followed by the node's value, then the right subtree's values
        traverseTree (Node value left right) = traverseTree left ++ [value] ++ traverseTree right

        --check if a list is sorted in ascending order
        isCorrectOrder :: (Ord a) => [a] -> Bool
        -- empty list is always in order
        isCorrectOrder [] = True
        -- one element of a tree will be in order as well
        isCorrectOrder [_] = True
        -- checks if each element is less than or equal to the next recusively
        isCorrectOrder (x:y:xs) = x <= y && isCorrectOrder (y:xs)


isBalanced :: AVLTree a -> Bool
-- checks for a balanced tree, empty tree is balanced 
isBalanced Empty = True
-- a tree is balanced for these conditions 
-- if the height difference between the left and right subtrees is at most 1
-- abs is for the absolute value
-- and both left and right subtrees are balanced 
-- otherwise the tree is not balaanced
isBalanced (Node nodeValue left right) |abs (heightAVL left - heightAVL right) <= 1 
                                       && isBalanced left && isBalanced right = True
                                       |otherwise = False




