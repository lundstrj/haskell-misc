-- Exam 2010
-- 14/8 2011
-- Johan Lundstroem

-- General
module Exam where
import Data.Char
-- import System.IO
import Test.QuickCheck
import Data.List
-- import Data.Maybe

-------------
-- Part #1 --
-------------

-- Task #1
findIndex_ :: Eq a => a -> [a] -> Int
findIndex_ elem list = findIndex' elem list 0

findIndex' :: Eq a => a -> [a] -> Int -> Int
findIndex' elem [] i = error "element not in list"
findIndex' elem (x:xs) i | x == elem = i
						 | otherwise = findIndex' elem xs (i+1)
			
-- Task #2			
extension :: String -> String
extension str = reverse (take ((findIndex_ '.' rev_str)+1) rev_str) where rev_str = reverse str

-- Task #3
data Form
    = And Form Form
    | Or Form Form
    | Not Form
    | Val Bool
	
eval :: Form -> Bool
eval (And a b) = eval a && eval b
eval (Or a b) = eval a || eval b
eval (Not a) = not (eval a)
eval (Val a) = a

-- Task #4
-- hmm, there is probably already 
prop_group :: Ord a => Eq a => [a] -> Bool
prop_group list = prop_group_2 (group list) && prop_group_1 list

prop_group_1 :: Eq a => [a] -> Bool
prop_group_1 list = (concat (group list) == list)

prop_group_2 :: Ord a => Eq a => [[a]] -> Bool
prop_group_2 list = all'' (prop_group_2' list)

prop_group_2' :: Ord a => Eq a => [[a]] -> [Bool]
prop_group_2' [] = [True]
prop_group_2' (x:xs) = [(all'' x)] ++ prop_group_2' xs

-- A slightly smaller all function. Not very fast though
all'' :: Ord a => Eq a => [a] -> Bool
all'' list = ((head list_) == (head (reverse list_))) where list_ = sort list

-- Task #5
yesNoQuestion :: String -> IO Bool
yesNoQuestion str = do
						putStrLn (show str)
						ans <- getLine
						let res = ans == "yes"
						return (res)
-------------
-- Part #2 --
-------------

-- Task #6
type Snake = [String]

testSnake :: [String]
testSnake = ["ahoy", "hola", "okay", "yahoo", "obrigado", "haskell"]

testSnake' :: [String]
testSnake' = ["george","michael","jackson","eminem"]

snake :: [String] -> Snake
snake list = longest [snakeHelper (delete x list) [x] | x<-list]

longest :: [Snake] -> Snake
longest list = longest' list []

longest' :: [Snake] -> Snake -> Snake
longest' [] tmp = tmp
longest' (x:xs) tmp | length x > length tmp = longest' xs x
					| otherwise = longest' xs tmp

snakeHelper :: [String] -> [String] -> Snake
snakeHelper list snak | hasMatch (head (reverse snak)) list [] =  snakeHelper a (snak++[b])
					  | otherwise = snak
					  where (a,b) = findMatch (head (reverse snak)) list []
					
hasMatch  :: String -> [String]	-> [String] -> Bool
hasMatch word (x:xs) ys = not (b == "None") where (a,b) = findMatch word (x:xs) ys
										 
findMatch :: String -> [String]	-> [String] -> ([String],String)
findMatch word [] ys = (ys,"None")
findMatch word (x:xs) ys | lastChar word == head x = (xs++ys,x)
						 | otherwise = findMatch word xs (ys++[x])

lastChar :: String -> Char
lastChar str = head (reverse str)

-- Task #7

data Diagram
	= Question String Diagram Diagram
	| Action String Diagram
	| Done
	
example :: Diagram
example =
    Question "Is it raining?"
      (Action "Buy an umbrella."
        (Question "Still getting wet?"
           (Action "Buy a rain coat." Done)
           Done))
      (Question "Is the sun shining?"
        (Action "Buy sun glasses."
          (Action "Go to the beach." Done))
        (Action "Stay at home." Done))

-- showDiagram :: Diagram -> String
-- showDiagram (Action str dia) = show str
-- showDiagram (Question str dia1 dia2) = show str

follow' :: Diagram -> [Diagram] -> IO [Diagram]
follow' (Done) list = do return (list)
follow' (Action str dia) list = do
							putStrLn (show str)
							follow' dia (list++[(Action str Done)])
follow' (Question str dia1 dia2) list = do 
									res <- yesNoQuestion str
									if res then
										do 
											-- putStrLn ("res="++(show res))
											-- putStrLn (showDiagram dia1)
											follow' dia1 list
									else
										do
											follow' dia2 list
				

		
follow :: Diagram -> IO [Diagram]
follow dia = follow' dia []
