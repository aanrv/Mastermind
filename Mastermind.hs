import Control.Monad (unless, when)
import System.Random (newStdGen, randomRs)
import System.Exit (exitSuccess)

sequenceLen		= 4 		:: Int
(minNum, maxNum)	= ('1', '6')	:: (Char, Char)

exitString	= ":q"			:: String
exitMessage	= "See you later!"	:: String

-- Generates a random sequence of length n between minNum and maxNum
generateSequence :: Int -> IO String
generateSequence n = do
	gen <- newStdGen
	return $ take n $ randomRs (minNum, maxNum) gen

-- Checks if user's answer is a valid in terms of length and character range.
-- Guess length must be equal to answer length and characters must be of the set [minNum..maxNum]
checkValidity :: String -> String -> IO Bool
checkValidity ans gues
	| length ans /= length gues = do
		putStrLn $ "Your guess must be of size " ++ show (length ans) ++ "."
		return False
	| otherwise = do
		let elemsMatch = elementsMatch [minNum..maxNum] gues
		when (not elemsMatch) $
			putStrLn $ "Each guess may only contain elements between " ++
			show minNum ++ " and " ++ show maxNum ++ "."
		return elemsMatch
  where
	elementsMatch :: (Eq a) => [a] -> [a] -> Bool
	elementsMatch range = foldr (\v c -> if not (elem v range) then c && False else c && True) True

-- Compares guess to answer.
-- Calulcates (correct elements in correct position, correct elements in wrong position)
compareSequence	:: String -> String -> (Int, Int)
compareSequence answer guess = (length answer - length ans, length ans - length allRemovedAns)
  where
	-- Given two lists, removes identical characters at the same index
	removeOneToOne :: (Eq a) => [a] -> [a] -> ([a], [a])
	removeOneToOne [] b = ([], b)
	removeOneToOne a [] = (a, [])
	removeOneToOne (a:as) (b:bs) = 	let alreadyRemoved = removeOneToOne as bs
					in if a == b then alreadyRemoved
					   else (a : fst alreadyRemoved, b : snd alreadyRemoved)

	-- Removes first occurence of an element in a list
	removeElem :: (Eq a) => a -> [a] -> [a]
	removeElem _ [] = []
	removeElem c (x:xs) = if c == x then xs else x : removeElem c xs

	-- Removes an instance of each character in the second string from the first
	removeContaining :: (Eq a) => [a] -> [a] -> [a]
	removeContaining answ [] = answ
	removeContaining answ (g:gs) =	if elem g answ then removeContaining (removeElem g answ) gs
					else removeContaining answ gs
	
	(ans, gss) 	= removeOneToOne answer guess
	allRemovedAns 	= removeContaining ans gss

displayRoundStats :: Int -> Int -> IO ()
displayRoundStats correctPlace wrongPlace = do
	putStrLn $ "Correct elements in the correct place:\t" ++ show correctPlace
	putStrLn $ "Correct elements in the wrong place:\t" ++ show wrongPlace

playGame :: String -> Int -> IO Bool
playGame answer round = do
	putStrLn $ "Round " ++ show round ++ 
		   "Enter a guess:"
	guess <- getLine
	
	if guess == exitString then do
		putStrLn exitMessage
		return True
	else do
		isValid <- checkValidity answer guess
		if not isValid
			then playGame answer round
		else do
			let (f,s) = compareSequence answer guess
			if f == length answer then do
				putStrLn "Correct!"
				return False
			else do
				displayRoundStats f s
				playGame answer $ round + 1

main = do
	answer <- generateSequence sequenceLen
	wantsToExit <- playGame answer 1
	
	unless wantsToExit $ do
	putStrLn "Would you not like to not play again? [y/n]"
	confusedResponse <- getLine
	
	if elem confusedResponse ["Y", "y"] then main
	else putStrLn exitMessage
	
