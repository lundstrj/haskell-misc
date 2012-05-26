-- Lab 3 (Sudoku)
-- 18/7 2011
-- Johan Lundstroem

-- General
module Sudoku where

import Data.Char
import System.IO
import Test.QuickCheck
import Data.List
import Data.Maybe

data Sudoku = Sudoku [[Maybe Int]]
    deriving (Show, Eq)

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

example :: Sudoku
example =
    Sudoku
      [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
      , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]

example_2 :: Sudoku
example_2 = 	  
	Sudoku 
	[[Just 2,Nothing,Nothing,Nothing,Nothing,Just 9,Nothing,Just 6,Nothing],
	[Just 4,Nothing,Just 9,Nothing,Just 5,Nothing,Nothing,Nothing,Nothing],
	[Just 7,Nothing,Nothing,Just 2,Nothing,Just 8,Nothing,Nothing,Just 9],
	[Just 5,Nothing,Nothing,Nothing,Nothing,Just 3,Nothing,Just 1,Nothing],
	[Nothing,Just 4,Nothing,Nothing,Nothing,Nothing,Nothing,Just 9,Nothing],
	[Nothing,Just 3,Nothing,Just 4,Nothing,Nothing,Nothing,Nothing,Just 7],
	[Just 6,Nothing,Nothing,Just 1,Nothing,Just 7,Nothing,Nothing,Just 3],
	[Nothing,Nothing,Nothing,Nothing,Just 2,Nothing,Just 6,Nothing,Just 8],
	[Nothing,Just 5,Nothing,Just 8,Nothing,Nothing,Nothing,Nothing,Just 1]]

example_sol :: Sudoku
example_sol = 
     Sudoku 
      [ [Just 3,Just 6,Just 4,Just 8,Just 7,Just 1,Just 2,Just 9,Just 5],
        [Just 7,Just 5,Just 2,Just 9,Just 3,Just 6,Just 1,Just 8,Just 4],
        [Just 8,Just 1,Just 9,Just 2,Just 5,Just 4,Just 7,Just 3,Just 6],
        [Just 5,Just 9,Just 6,Just 7,Just 1,Just 3,Just 4,Just 2,Just 8],
        [Just 4,Just 3,Just 1,Just 5,Just 8,Just 2,Just 6,Just 7,Just 9],
        [Just 2,Just 7,Just 8,Just 4,Just 6,Just 9,Just 3,Just 5,Just 1],
        [Just 6,Just 4,Just 5,Just 3,Just 2,Just 8,Just 9,Just 1,Just 7],
        [Just 9,Just 8,Just 3,Just 1,Just 4,Just 7,Just 5,Just 6,Just 2],
        [Just 1,Just 2,Just 7,Just 6,Just 9,Just 5,Just 8,Just 4,Just 3]]
      
example' :: Sudoku
example' =
    Sudoku
      [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
      , [Just 7, Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]
      
example'' :: Sudoku
example'' =
    Sudoku
      [ [Just 3, Just 6, Just 4, Just 8, Just 7, Just 1, Just 2, Just 9,Just 5]
      , [Just 7, Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]
      
-- Task #A1
--{-| 

-- Will give you a completely blank Soduko
allBlankSudoku :: Sudoku
allBlankSudoku = (Sudoku [allBlankRow | x<-[1..9]])

-- Will give you a completely blank Soduko row
allBlankRow :: [Maybe Int]
allBlankRow = [Nothing | x<-[1..9]]

-- Task #A2
isSudoku :: Sudoku -> Bool
isSudoku chart = (isSudokuValuesRow (rows chart)) && (size (rows chart)) == 9

isSudokuValuesRow :: [[Maybe Int]] -> Bool
isSudokuValuesRow [] = True
isSudokuValuesRow (row:xs) = (((allTrue (isSudokuValues' row)) && size row == 9) && (isSudokuValuesRow xs))

isSudokuValues' :: [Maybe Int] -> [Bool]
--isSudokuValues' list = [True | x<-list, ((((Just x)> 0) && ((Just x)<= 9)) || x == Nothing)]
isSudokuValues' list = [True | x<-list, valueOk x]

-- We need to convert the Maybe type into something we can examine
valueOk Nothing = True
valueOk x = ((fromMaybe' x) > 0) || ((fromMaybe' x) < 10)

fromMaybe' :: Maybe a-> a
fromMaybe' (Just x) = x

fromMaybe'Str :: Maybe Int -> Char
fromMaybe'Str Nothing = '.'
fromMaybe'Str (Just x) = intToDigit x

allTrue :: [Bool] -> Bool
allTrue [True] = True
allTrue (x:xs) | x == True = allTrue xs
             | otherwise = False
             
size :: [a] -> Integer
size [] = 0
size (x:xs) = 1 + size xs

-- Task #A3
test :: [Maybe Int]
test = [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]

isSolved :: Sudoku -> Bool
isSolved chart = (isNumValuesRow (rows chart))

isNumValuesRow :: [[Maybe Int]] -> Bool
isNumValuesRow [] = True
isNumValuesRow (row:xs) = (((allTrue (isNumValues' row))) && (isNumValuesRow xs))

valueNum :: Maybe Int -> Bool
valueNum a = not (a == Nothing)

isNumValues' :: [Maybe Int] -> [Bool]
isNumValues' list = [valueNum x | x<-list]

-- Crap. I didn't read the hints. I just started hacking. Well, it works!

-- Task #B1
-- IO

printSudoku :: Sudoku -> IO ()
printSudoku chart = do printSudoku' (rows chart)

printSudoku' :: [[Maybe Int]] -> IO ()
printSudoku' [] = return ()
printSudoku' (x:xs) | size (x:xs) > 0 = do printSudokuRow x
                                           printSudoku' xs

printSudokuRow :: [Maybe Int] -> IO ()
printSudokuRow [] = do putStrLn ""
printSudokuRow (x:xs) = do putChar (fromMaybe'Str x)
                           printSudokuRow xs
                           
-- Task #B2
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do 
              file_handle <- openFile path ReadMode
              file_content <- hGetContents file_handle
              let rows = lines file_content
              --putStr file_content
              --makeSudoku rows
              return (makeSudoku rows)

              
--makeSudoku :: [String] -> Sudoku
makeSudoku dat = (Sudoku [makeSudokuHelper x | x <- dat])

makeSudokuHelper :: String -> [Maybe Int]
makeSudokuHelper xs = [toMaybe x | x <- xs]

toMaybe :: Char -> Maybe Int
toMaybe x | x == '.' = Nothing
          | otherwise = Just (digitToInt x)
          
          
-- Task #C1
-- How friggin cool is this?
cell :: Gen (Maybe Int)
cell = oneof[return Nothing, 
             do r <- choose(1,9)
                return (Just r)]
                
-- Task #C2
-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)
       
-- Task #C3
prop_sudoku :: Sudoku -> Bool
prop_sudoku x = isSudoku x

-- Task #D1
type Block = [Maybe Int]

okBlock :: Block
okBlock = [Just 1, Just 7, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 2]
nokBlock :: Block
nokBlock = [Just 1, Just 7, Nothing, Just 7, Just 3, Nothing, Nothing, Nothing, Just 2]

isOkayBlock :: Block -> Bool
isOkayBlock block = isOkayBlockHelper block

isOkayBlockHelper block = (size (stripNothing block) == size (stripNothing(nub block))) && (size block == 9)

-- I used a list comprehension!
stripNothing :: Block -> [Maybe Int]
stripNothing (x:xs) = [x | x <- xs, valueNum x]

-- Task #D2

blocks :: Sudoku -> [Block]
blocks chart = rows chart ++ transpose (rows chart) ++ blockify chart

blockify :: Sudoku -> [Block]
blockify chart = blockify' (rows chart)

blockify' :: [[Maybe Int]] -> [Block]
blockify' [[]] = []
blockify' chart | size chart == 0 = []
                | otherwise = blockifyRows (take 3 chart) ++ blockify' (drop 3 chart)
               
blockifyRows :: [[Maybe Int]] -> [Block]
blockifyRows [] = []
blockifyRows (x:y:z:xs) | x == [] = []
                        | otherwise = [take 3 x ++ take 3 y ++ take 3 z] ++ blockifyRows [(drop 3 x),(drop 3 y),(drop 3 z)]

prop_blocks :: Sudoku -> Bool
prop_blocks chart = (size (blocks chart) == 3*9) && prop_blocks' chart

prop_blocks' :: Sudoku -> Bool
prop_blocks' chart = allTrue [(size x == 9) | x <-(blocks chart)]

prop_blocks'' :: Sudoku -> Bool
prop_blocks'' chart = allTrue [isOkayBlock x | x <-(blocks chart)]

-- Task #D3
isOkay :: Sudoku -> Bool
isOkay chart = prop_blocks chart && prop_blocks'' chart


-- Task #E1
type Pos = (Int, Int)

isBlank :: Sudoku -> Pos -> Bool
isBlank chart (row,col) = (!!)((!!) (rows chart) row) col == Nothing

cellValue :: Sudoku -> Pos -> Maybe Int
cellValue chart (row,col) = (!!)((!!) (rows chart) row) col

blank :: Sudoku -> Pos
blank chart = blank' (0,0) (rows chart)

noValue :: Pos
noValue = (-1,-1)

blank' :: (Int, Int) -> [[Maybe Int]] -> Pos
blank' (row,col) [] = noValue
blank' (row,col) roww | not (res == -1) = (col,res)
                 | otherwise = blank' (0,(row+1)) (drop 1 roww)
                 where res = (searchRow 0 (head(take 1 roww)))
                 
searchRow :: Int -> [Maybe Int] -> Int
searchRow x [] = -1
searchRow x (y:ys) | y == Nothing = x
                   | otherwise = searchRow (x+1) ys
                   
-- Task #E2
-- will update the list at pos Int with elem a.
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) list (i,x) = helper list (i,x) 0

helper :: [a] -> (Int,a) -> Int -> [a]
helper [] (i,x) index = []
helper list (i,x) index | index == i = [x] ++ helper (drop 1 list) (i,x) (index+1)
                        | otherwise = (take 1 list) ++ helper (drop 1 list) (i,x) (index+1)
                        
-- Task #E3
-- Un f-ing belivable. It actually works
-- update is OK
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update chart (row,col) new = (Sudoku (updateHelper (rows chart) (row,col) new 0 0))

updateHelper :: [[Maybe Int]] -> Pos -> Maybe Int -> Int -> Int -> [[Maybe Int]]
updateHelper the_rows (row,col) new row_count col_count | row == row_count = [(!!=)(head (take 1 the_rows)) (col,new)] ++ (drop 1 the_rows)
                                                | otherwise = (take 1 the_rows) ++ updateHelper (drop 1 the_rows) (row,col) new (row_count+1) col_count
                                                
{- 
[(!!=) ((head(take 1 the_rows)) (y, new))] ++ drop 1 the_rows 
-}

-- Task #F1
test_ :: Sudoku -> IO()
test_ chart = do putStrLn (show (findGoodCells chart));
			     printSudoku (fixOneCell chart);
				 test_ (fixOneCell chart);

-- Solve works.
solve :: Sudoku -> Maybe Sudoku
solve chart | not(findFirstGoodCell chart == (-2,-2)) = solve (fixOneCell chart)
            | isSolved chart = (Just chart)
            | (findFirstGoodCell chart == (-2,-2)) && not (isSolved chart) = Nothing

fixOneCell :: Sudoku -> Sudoku
fixOneCell chart | pos == (-2,-2) = chart
                 | otherwise = update chart pos (head(allowedValues chart pos))
                  where pos = (findFirstUsableCell chart)

options :: [Maybe Int]
options = [Just x | x<-[1..9]]

allowedEntry :: Sudoku -> Pos -> Maybe Int -> Bool
allowedEntry chart (row,col) new = (isOkay (update chart (row,col) new)) && isBlank chart (row,col)

onlyOnePossible :: Sudoku -> Pos -> Bool
onlyOnePossible chart (row,col) = size (allowedValues chart (row,col))== 1 

allowedValues :: Sudoku -> Pos -> [Maybe Int]
allowedValues chart (row,col) = [z | z <- options, allowedEntry chart (row,col) z, isBlank chart (row,col)]

findFirstGoodCell :: Sudoku -> Pos
findFirstGoodCell chart = head (findAllGoodCells allCoordinates chart)

-- findViolator :: Sudoku -> Pos

findFirstUsableCell :: Sudoku -> Pos
findFirstUsableCell chart | pos == (-2,-2) = blank chart
						  | otherwise = pos
						  where pos = findFirstGoodCell chart

firstGood :: [Pos] -> Sudoku -> Pos
firstGood [] chart = (-2,-2)
firstGood (x:xs) chart | onlyOnePossible chart (x) = (x)
                       | otherwise = firstGood xs chart

findGoodCells :: Sudoku -> [Pos]
findGoodCells chart = findAllGoodCells allCoordinates chart
					   
findAllGoodCells :: [Pos] -> Sudoku -> [Pos]
findAllGoodCells [] chart = [(-2,-2)]
findAllGoodCells (x:xs) chart | onlyOnePossible chart (x) = [x] ++ findAllGoodCells xs chart
                              | otherwise = findAllGoodCells xs chart

allCoordinates :: [(Int, Int)]
allCoordinates = concat [(zip (allHelper x) [0..8]) | x<-[0..8]]

allHelper ::Int -> [Int]
allHelper x = [x | y<-[0..8]]

-- Task #F2
readAndSolve :: FilePath -> IO ()
readAndSolve path = do sud <- readSudoku path;
                       printSudoku (fromJust (solve sud))
                       
-- Task #F3
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sol chart = (isSolved sol) && (isSubset sol chart)

isSubset :: Sudoku -> Sudoku -> Bool
isSubset sol chart = allTrue [(cellCompOk (cellValue sol (row,col)) (cellValue chart (row,col))) | (row,col)<-allCoordinates]

cellCompOk :: Maybe Int -> Maybe Int -> Bool
cellCompOk sol chart | chart == Nothing = True
                     | chart == sol = True
                     | otherwise = False
                     
compSudoku :: FilePath -> FilePath -> IO ()
compSudoku path_sol path_chart = do sol <- readSudoku path_sol
                                    chart <- readSudoku path_chart
                                    let res = isSolutionOf sol chart
                                    -- putStrLn (show sol)
                                    putStrLn (show res)
                                    
prop_SolveSound :: Sudoku -> Bool
prop_SolveSound chart = isSolutionOf sol chart where sol = (fromJust (solve chart))