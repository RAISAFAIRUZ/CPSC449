{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
-- Question 1
transposeMatrix :: [[a]] -> [[a]]
transposeMatrix [] = [] -- base case for empty list, returns empty list
transposeMatrix ([] : _) = [] -- if first row's list is empty, returns empty list
transposeMatrix n = headOfList n : transposeMatrix (tailOfList n) -- recursive calls to create new transpose matrix
-- creates lists with head of each list, creates another list recursively
  where
    headOfList [] = [] -- base case for empty list
    headOfList ((x : xs) : otherList) = x : headOfList otherList -- extracts head of all the lists
    tailOfList [] = [] -- base case for empty list
    tailOfList ((x : xs) : otherList) = xs : tailOfList otherList -- gets rid of the head and keeps the tail for all the lists

-- Question 2
groupByPredicate :: (a -> Bool) -> [a] -> [[a]]
groupByPredicate _ [] = [] -- function and empty list input, returns empty list
groupByPredicate boolean (x : xs) = currentList boolean (boolean x) [x] xs -- create new list that satisfies the function keeping the rest aside

-- Helper function for concatanation at the end of the list
addFunc :: [a] -> a -> [a]
addFunc [] y = [y] -- add y to empty list
addFunc (z : zs) y = z : addFunc zs y -- recursively add y to the end of the list

currentList :: (a -> Bool) -> Bool -> [a] -> [a] -> [[a]]
currentList _ _ x [] = [x] -- add to empty list
currentList boolean predicateBool x (y : ys)
  | boolean y == predicateBool = currentList boolean predicateBool (addFunc x y) ys --- if y matches the predicate function, add it to the current list
  | otherwise = x : currentList boolean (boolean y) [y] ys -- otheriwse start new list

-- Question 3

movies :: [Movie]
movies =
  [ Movie "Inception" 8.8 "Sci-Fi",
    Movie "The Matrix" 8.7 "Sci-Fi",
    Movie "Jaws" 7.9 "Thriller",
    Movie "Frozen" 7.4 "Animation",
    Movie "Interstellar" 8.6 "Sci-Fi",
    Movie "The Godfather" 9.7 "Crime",
    Movie "Toy Story" 8.3 "Animation"
  ]

data Movie = Movie {title :: String, rating :: Float, genre :: String} deriving (Show)

topRatedMovies :: [Movie] -> Float -> [String]
topRatedMovies movies inputRate = map title (filter (\x -> rating x > inputRate) movies) -- maps through title to filter out the ratings according to the input rating, lamda used

moviesByGenre :: [Movie] -> String -> [String]
moviesByGenre movies inputGenre = map title (filter (\x -> genre x == inputGenre) movies) -- maps through title to filter out the movies according to the input genre, lamda used

adjustedRatings :: [Movie] -> [Movie]
adjustedRatings =
  map -- maps through all the movies
    ( \movie ->
        Movie
          (title movie) -- movie title
          (min 10.0 (rating movie + 0.5)) -- increase rating by 0.5, not exceeding 10.0
          (genre movie) -- genre of the movie
    )

-- Question 4
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = [] -- empty list is sorted
mergeSort [x] = [x] -- one element list is sorted
mergeSort list = merge (mergeSort left) (mergeSort right) -- divide lists in halves, the sort the halves and merge them together
  where
    (left, right) = divide list -- divide a list into left and right

divide :: [a] -> ([a], [a])
divide list = splitHalf list [] [] -- take one list and split it in half make it two lists

splitHalf :: [a] -> [a] -> [a] -> ([a], [a])
splitHalf [] left right = (left, right) -- base case for two lists
splitHalf [x] left right = (x : left, right) -- if odd elements add it to the left list
splitHalf (x : y : xs) left right = splitHalf xs (x : left) (y : right) -- distribute elements between the two lists

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys -- if the first list is empty, return the other list
merge xs [] = xs -- if the second list id empty just return the first list
merge (x : xs) (y : ys) -- process to merge the two lists with sorting them
  | x <= y = x : merge xs (y : ys) -- if the head of the first list smaller than the other, put it as head, repeat the process
  | otherwise = y : merge (x : xs) ys -- if the other list's head is bigger put it as head then continue to merge the rest recursively
