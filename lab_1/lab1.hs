-- Lab 1
-- 25/4 2011
-- Johan Lundstroem

module Lab1 where

-- Reference information
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Assignment 1
-- steps n = n+1

-- Assignment 2
---------You forgot base case and negative case here.  make_list in Haskell is a standart function "replicate".
power1 :: Integer -> Integer -> Integer
power1 n k = product(make_list k n)

make_list :: Integer -> Integer -> [Integer]
make_list size item = [item | y <- [1..size]]

-- Assignment 3
---------isEven is Haskell standart function "even". isEven(k+1) can be just another case of "if", written as otherwise
---power2 n k | even k = power2 (n*n) (div k 2)
---		      | otherwise = n * power2 n (k-1)
power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k | isEven(k) = power2 (n*n) (k`div`2)
power2 n k | isEven(k+1) = n * power2 n (k-1)

isEven :: Integer -> Bool
isEven k = k `mod` 2 == 0

-- Assignment 4
-- A
--  I need even and odd k to test power2 properly

comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k = power n k == power1 n k

comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k = power n k == power2 n k

---Why haven't you made a function, which will return single Bool?
---comparePowers :: [(Integer, Integer)] -> Bool 
---You can use functions "and" and "map" for that, the tuple (Bool, Bool) is not exactly the right answer.
comparePowers :: [(Bool, Bool)]
comparePowers = [(comparePower2 x y, comparePower1 x y)| (x,y)<-test_tuples]

test_tuples = [(2,2),(3,3),(3,4),(4,3)]

-- Assignment 4
-- (No internet connection, cannot get MkHTML.hs will try anyway)  
-- I need to figure out sequential execution

---Makes very little sense without MkHTML.hs :) Don't think you need sequantial execution with help functions it provides.
---You should try again with this library ;)

makeHtml :: Integer -> Integer -> [(Integer, Integer, Integer, Integer)]
makeHtml n 0 = [(0,1,1,1)]
makeHtml n k = makeHtmlRow n k : makeHtml n (k-1)

makeHtmlRow :: Integer -> Integer -> (Integer, Integer, Integer, Integer)
makeHtmlRow n k = (k, power n k, power1 n k, power2 n k)

powOfTwo :: Integer -> [(Integer, Integer, Integer, Integer)]
powOfTwo k = reverse (makeHtml 2 k)

prefixPowOfTwo k = ("k","power","power1","power2") : [(show a, show b, show c, show d)|(a,b,c,d)<-(powOfTwo k)]

moo k = do putStrLn (foo (prefixPowOfTwo k))
--"k\tpower\tpower1\tpower2\n"
foo [] = show "end of data"
foo ((a,b,c,d):xs) = show a++"\t"++show b++"\t"++show c++"\t\t"++show d++"\n"++ foo xs
