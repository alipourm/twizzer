import Control.Monad.State

{-
 -
 - CS583 Functional Programming
 - Twizzer project
 - Group members: Qi Qi, Amin Alipour, Sean Moore, Qingkai Lu
 -
-}


--How to run this program
--1, cope this file into a directory
--2, compile this file using command under terminal: ghc --make program.hs
--3, type in terminal: ./program
--4, Currently, this program can only store twizes, use command: submit name solution

type Twize = (String, String)

--main function for program entrance
main = loop []

--loop function is used to parse command, and do state transformation
loop :: [Twize] -> IO ()
loop state = do
			line <- getLine
			let (command, argu1, argu2) = parse line
			if (command == "submit")
			then do
				let (result, newState) = runState (submit argu1 argu2) state
				putStrLn . show $ newState
				loop newState
			else
				putStrLn "stop"


--parse function is used to parse command-line
--needs to add more command
parse :: String -> (String, String, String)
parse line
		| argc /= 3 = ("", "", "")
		| otherwise = ((words line) !! 0, (words line) !! 1, (words line) !! 2)
		where argc = length . words $ line


--state transformer monad
submit :: String -> String -> State [(String, String)] ()
submit name solutions = do
						xs <- get
						put $ (name, solutions):xs
