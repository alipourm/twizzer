{-
 - CS 583 Functional Programming
 - Twizzer Project
 - Twizzer group 3
 - Group members: Qi Qi, Amin Alipour, Qingkai Lu
-}

import System.IO
import System.Directory
import System.Posix
--import System.Posix.Files
import System.Locale
import System.Time
--import System.Random  --Random module not found in Haskell 7.4.1
import Data.Time
import Data.Time.Format
import Data.Time.Clock

--data Handler = 

main :: IO ()
main = do
		line <- getLine
		let commands = parseCommand line
		if commands !! 0 == "stop"
		then return ()
		else do
				execute commands
				main


execute :: [String] -> IO ()
execute all@(command:argus)
				| command == "setUp" = setUpHandler argus -- admin 
				| command == "nextStep" = nextStepHandler argus-- putStrLn "nextStep" -- admin
				| command == "showTwizze" = putStrLn "showTwizz" --showTwizze-- user run this command after admin has set up the twizze homework
				| command == "showBuddyTwizze" = showBuddyTwizze -- Now assume 1 on 1 //. read . head $ argus
				| command == "submitTwizze" = twizzeHandler argus--putStrLn "submitTwizz" --twizzeHandler argus -- user wants to submit their solution
				| command == "submitReview" = putStrLn "submitReview" --reviewHandler argus -- user
				| otherwise = putStrLn "wrong command, please try again"



parseCommand :: String -> [String]
parseCommand commands = words commands

--administrator sets up twizzer system
--arguments should follow: deadline randomSeed twizzefileName
--there should be a configuration file called "names.txt", which stores dealline for current twizze, and students names.
--fisrt, setUpHandler reads names from names.txt, and assigns buddies for each student, and store buddies information in another file,
--called buddy.txt
--
--
setUpHandler :: [String] -> IO ()
setUpHandler argus = do
						inh <- openFile "twizze/names" ReadMode
						outh <- openFile "twizze/buddy" WriteMode
						createConfig argus
						System.Directory.createDirectory ("twizze/" ++ (argus !! 1)) 
						setFileMode ("twizze/" ++ (argus !! 1)) 0o757
						names <- readName inh [] --read all students names from file, and store names in varibal names, which is a list of string
						randomNames <- shuffleList names -- shuffle name list, making it randomly.
--						putStrLn (concat randomNames)
						_ <- writeBuddies outh randomNames -- write those shuffled names into a file.
						hClose inh
						hClose outh


--first argument is deadline
--second argument is twizze name
createConfig :: [String] -> IO ()
createConfig argus = do
						outh <- openFile "twizze/config" WriteMode
						hPutStrLn outh "uploadTwizze" -- current state
						hPutStrLn outh (argus !! 0) -- deadline
						hPutStrLn outh (argus !! 1) -- twizze name
						hClose outh



--readName function reads all students names from input handler, it recursively call itself.
--this function not only reads all names from configuration file, it also create folder for each name. 
readName :: Handle -> [String] -> IO [String]
readName inh names = do
				ineof <- hIsEOF inh
				if ineof
				then return names
				else do
						name <- hGetLine inh
						configs <- readAllConfig
						let twizzeName = (configs !! 2)
						isExist <- fileExist ("twizze/" ++ twizzeName ++ "/" ++ name)
						if (isExist == True)
						then readName inh (name:names)
						else do
							System.Directory.createDirectory ("twizze/" ++ twizzeName ++ "/" ++ name) --create a folder for every student.
							setFileMode ("twizze/" ++ twizzeName ++ "/" ++ name) 0o757
							readName inh (name:names)
				

-- write buddy information into a file
writeBuddies :: Handle -> [String] -> IO ()
writeBuddies outh [] = return ()
writeBuddies outh (n:names) = do
								hPutStrLn outh (n ++ " " )
								writeBuddies outh names

{-

writeLoop :: Handle -> Handle -> IO ()
writeLoop inh outh = do
						ineof <- hIsEOF inh
						if ineof
						then return ()
						else do intStr <- hGenLine inh
								hPutStrLn outh 
						

-}

--client submit twizze, argu1 should be path
twizzeHandler :: [String] -> IO ()
twizzeHandler argus = do
						uid <- getUID
						if (not . isRegistered $ uid)
						then putStrLn "sorry, you are not registed in this class."
						else do
								if(not (curState == "uploadTwizze"))
								then putStrLn "sorry, you can not submit twizze now."
								else do
										if(alreadySubmitted uid)
										then return ()--change old twizze's name to twizzeX_old.hs
										else do
												submittwizze . head $ argus
												putStrLn "upload successfully"




--call system function
getUID :: IO String
getUID = getEffectiveUserName

--whether current user is registed in this class
--need to be refactored
isRegistered :: String -> Bool
isRegistered uid = True

--check whether currentet state allows students to submit their twizzes.
--need to be refactored
curState = "uploadTwizze"

alreadySubmitted :: String -> Bool
alreadySubmitted uid = False

--readTwizze
--
--
--
--
-- user need use this command to submit their twizzer homeworks
submittwizze :: String -> IO ()
submittwizze path = do
					uid <- getUID
					configs <- readAllConfig
					let twizzeName = configs !! 2
					copyFile path ("../../q/qiq/twizze/" ++ twizzeName ++ "/" ++ uid ++ "/" ++ twizzeName)
					setFileMode ("../../q/qiq/twizze/" ++ twizzeName ++ "/" ++ uid ++ "/" ++ twizzeName) 0o755
					putStrLn "submit homework successfully"
				--need to copy twizze file from student directory to administrator's directory
				



showBuddyTwizze :: IO ()
showBuddyTwizze = do
					configs <- readAllConfig
					let state = head configs
					let twizzeName = (configs !! 2)
					if (not (state == "submitReview"))
					then putStrLn "sorry, you can not see buddies homework at this stage"
					else do
							uid <- getUID
							if (not . isRegistered $ uid)
							then putStrLn "sorry, you are not registred in this class"
							else do
									listBuddies <- readBuddies -- read all buddies from file buddy
									let buddy = findBuddy uid listBuddies -- look for current users buddy
									content <- readFile ("twizze/" ++ twizzeName ++ "/" ++ buddy ++ "/" ++ twizzeName) -- read buddies's homework and print out
									putStrLn content



findBuddy :: String -> [(String, String)] -> String
findBuddy uid listBuddy = "qiq"


readBuddies :: IO [(String, String)]
readBuddies = do
				return [("a", "b")]


{--
reviewHandler :: [String] -> IO ()
reviewHandler argus = do

--xxxHandler :: IO ()



parseCommand :: String -> [String]
parseCommand commandLine = 




showTwizze :: IO ()
showTwizze = do
			if (not isRegistered(getUID))
			then putStrLn "sorry"
			else do
				if (not existFile(twizzefileName))
				then putStrLn "sorry"
				else putStrLn twizzefile


passDeadline :: IO Bool
passDeadline = do
				dateString <- getLine
				let deadline = readTime defaultTimeLocale "%d %b %Y %l:%M %p" dateString :: UTCTime
				currentTime <- getCurrentTime --IO UTCTime 
				let timeDiffInSeconds = floor (toRational (diffUTCTime deadline currentTime))
				if timeDiffInSeconds > 0
				then putStrLn "false"
				else putStrLn "true"


--}
-- this function is used to shuffle a list randomly
shuffleList :: [a] -> IO [a]
shuffleList [i] = return [i]
shuffleList xs = do
					ran <- getRandom (length xs - 1)
					let front = (xs !! ran)
					list <- shuffleList (take ran xs ++ drop (ran + 1) xs)
					return (front : list)

  


getRandom :: Int -> IO Int
getRandom range = do
			time <- getCurrentTime
			let ran = (floor . toRational . utctDayTime $ time) `mod` range
			return ran

allStates = ["uploadTwizze", "submitReview", "seeReview"]


findNextState :: String -> String
findNextState currentState 
						| currentState == "uploadTwizze" = "submitReview"
						| currentState == "submitReview" = "seeReview"
						| currentState == "seeReview" = "seeReview"

-- nextStep run by admin to switch state
-- basiclly it is used to change config file.
nextStepHandler :: [String] -> IO ()
nextStepHandler argus = do
					configs <- readAllConfig
					let nextState = findNextState (configs !! 0) -- compare with allStates
					outh <- openFile "config" WriteMode
					if nextState == "submitReview"
					then do
						hPutStrLn outh nextState
						hPutStrLn outh (argus !! 0)
						hPutStrLn outh (configs !! 2)
					else do
						hPutStrLn outh nextState
						hPutStrLn outh "none"
						hPutStrLn outh (configs !! 2)
					hClose outh



readAllConfig :: IO [String]
readAllConfig = do
					inh <- openFile "twizze/config" ReadMode
					state <- hGetLine inh
					deadline <- hGetLine inh
					twizzeName <- hGetLine inh
					hClose inh
					return [state, deadline, twizzeName]


