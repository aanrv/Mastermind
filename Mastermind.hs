import Control.Monad (unless, when)
import System.Random (newStdGen, randomRs)

-- Length of sequence
sequenceLen	= 4 :: Int
-- Bounds of possible elements in sequence
(minNum,maxNum)	= ('1', '6') :: (Char, Char)

-- The expected response to indicate boredom
exitString	= ":q"
-- A heartwarming message to display on exit
exitMessage	= "Good riddance noob."

-- Generates a random sequence of length n 
-- containing elements between minNum and maxNum
generateSequence :: Int -> IO String
generateSequence n = do
	gen <- newStdGen
	return $ take n $ randomRs (minNum, maxNum) gen

-- Checks if user's answer is a valid in terms of length and character range.
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
	elementsMatch range =
		foldr (\v c -> if not (elem v range) then c && False else c && True) True

-- Compares guess to answer.
-- Calulcates (correct elements in correct position, correct elements in wrong position)
compareSequence	:: String -> String -> (Int, Int)
compareSequence answer guess =
	(length answer - length ans, length ans - length allRemovedAns)
  where
	-- Given two lists, removes identical characters at the same index
	removeOneToOne :: (Eq a) => [a] -> [a] -> ([a], [a])
	removeOneToOne [] b = ([], b)
	removeOneToOne a [] = (a, [])
	removeOneToOne (a:as) (b:bs) =
		let alreadyRemoved = removeOneToOne as bs
		in if a == b then alreadyRemoved
		   else (a : fst alreadyRemoved, b : snd alreadyRemoved)

	-- Removes first occurence of an element in a list
	removeElem :: (Eq a) => a -> [a] -> [a]
	removeElem _ [] = []
	removeElem c (x:xs) = if c == x then xs else x : removeElem c xs

	-- Removes an instance of each character in the second list from the first
	removeContaining :: (Eq a) => [a] -> [a] -> [a]
	removeContaining answ [] = answ
	removeContaining answ (g:gs) = if elem g answ
				       then removeContaining (removeElem g answ) gs
				       else removeContaining answ gs
	
	(ans, gss) 	= removeOneToOne answer guess
	allRemovedAns 	= removeContaining ans gss

-- Notify the user of their guess's accuracy
displayRoundStats :: Int -> Int -> IO ()
displayRoundStats correctPlace wrongPlace = do
	putStrLn $ "Correct elements in the correct place:\t" ++ show correctPlace
	putStrLn $ "Correct elements in the wrong place:\t" ++ show wrongPlace

playGame :: String -> Int -> IO Bool
playGame answer round = do
	putStrLn $ "Round " ++ show round ++ 
		   "\nEnter a guess:"
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
		-- I think I'm funny
		putStrLn "Would you not like to not play again? [y/n]"
		confusedResponse <- getLine
	
		if elem confusedResponse ["Y", "y"]
		then main
		else putStrLn exitMessage
	
