module HelloWorld where
import Data.Char

toFive :: Char -> Char
toFive a = '5'

main2 :: IO ()
main2 = do
	putStrLn "Hi there, what is your name?"
	name <- getLine
	putStrLn ("Hey "++name++", you rock")
	let olle = map toFive "FooBaar"
	if ((head name) == 'Q') then 
			do 
			putStrLn "Mooo"
			putStrLn ("Quitting")
	else
		do
		putStrLn olle
		main
		
--myShow :: String -> IO ()
--myShow x = do 
		
main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn (reverseWords line  )
            main  
  
reverseWords :: String -> String  
reverseWords = unwords . reverse . words