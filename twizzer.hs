{-
 - CS 583 Functional Programming
 - Twizzer Project
 - Twizzer group 3
 - Group members: Qi Qi, Amin Alipour, Qingkai Lu
-}

import System.IO
--import System.Directory
import System.Posix
--import System.Posix.Files
import System.Locale
import System.Time
import System.Random  --Random module not found in Haskell 7.4.1
import Data.Time
import Data.Time.Format
import Data.Time.Clock

--data Handler =

main :: IO ()
main = do
         line <- getLine
         let commands = parseCommand line
         if commands !! 0 == "exit"
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
				| command == "submitReview" = submitReview (head argus) --putStrLn "submitReview" --reviewHandler argus -- user
				| command == "seeReview" = seeReview
				| otherwise = putStrLn "wrong command, please try again"



parseCommand :: String -> [String]
parseCommand commands = words commands

--administrator sets up twizzer system
--arguments should follow: deadline randomSeed twizzefileName
--there should be a configuration file called "names.txt", which stores dealline for current twizze, and students names.
--fisrt, setUpHandler reads names from names.txt, and assigns buddies for each student, and store buddies information in another file,
--called buddy.txt
{-
setUpHandler :: [String] -> IO ()
setUpHandler argus = do
						--inh <- openFile "twizze/names" ReadMode
                        --names <- readName
						--outh <- openFile "twizze/buddy" WriteMode
						createConfig argus --create configuration file, every setup will generate a new config file
                        createProject
						--System.Directory.createDirectory ("twizze/" ++ (argus !! 1))
						--setFileMode ("twizze/" ++ (argus !! 1)) 0o757
						names <- readName --read all students names from file, and store names in varibal names, which is a list of string
                        --createFolders names
                        buildBuddies names
						--let randomNames <- shuffleList names -- shuffle name list, making it randomly.
--						putStrLn (concat randomNames)
						--let chgRandNames = (tail randomNames) ++ [(head randomNames)]
						--_ <- writeBuddies outh chgRandNames  randomNames -- write those shuffled names into a file.
						--hClose inh
						--hClose outh

-}

setUpHandler :: [String] -> IO ()
setUpHandler argus = do
                       createConfig argus --create configuration file, every setup will generate a new config file
                       createProject
                       names <- readName --read all students names from file, and store names in varibal names, which is a list of string
                       buildBuddies names


createProject :: IO ()
createProject = do
                  configs <- readAllConfig
                  createDirectory ("twizze/" ++ (configs !! 2)) 0o757-- create folder for current project
                  --setFileMode ("twizze/" ++ (configs !! 2)) 0o757
                  names <- readName
                  createFolders ("twizze/" ++ (configs !! 2) ++ "/") names 0o757


createFolders :: String -> [String] -> FileMode -> IO ()
createFolders _ [] _ = return ()
createFolders path (x:xs) mode = do
                              createDirectory (path ++ x) mode
                              --setFileMode ("twizze/" ++ x) mode
                              createFolders path xs mode


buildBuddies :: [String] -> IO ()
buildBuddies names = do
                       outh <- openFile "twizze/buddy" WriteMode
                       ran <- getRandom 60
                       let randomNames = randPerm (mkStdGen ran) names
                       let randomNamesShiftByOne = (tail randomNames) ++ [(head randomNames)]
                       writeBuddies outh randomNamesShiftByOne randomNames
                       hClose outh





randPerm :: StdGen -> [a] -> [a]
randPerm _ [] = []
randPerm gen xs = let (n, newGen) = randomR (0, length xs - 1) gen
                      front = xs !! n
                  in front : randPerm newGen (take n xs ++ drop (n+1) xs)




--first argument is deadline
--second argument is twizze name
createConfig :: [String] -> IO ()
createConfig argus = do
						outh <- openFile "twizze/config" WriteMode
						hPutStrLn outh "uploadTwizze" -- current state
						hPutStrLn outh (argus !! 0) -- deadline
						hPutStrLn outh (argus !! 1) -- twizze name
						hClose outh


{-
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

-}

readName :: IO [String]
readName = do
             content <- readFile "twizze/names"
             return . words $ content


-- write buddy information into a file
writeBuddies :: Handle -> [String] -> [String]  -> IO ()
writeBuddies outh [] _ = return ()
writeBuddies outh _ [] = return ()
writeBuddies outh (n:names)(sn:shuffledN) = do
								hPutStrLn outh ( n ++ " " ++ sn )
								--hPutStrLn outh (sn)
								writeBuddies outh names shuffledN





--client submit twizze, argu1 should be path
twizzeHandler :: [String] -> IO ()
twizzeHandler argus = do
						uid <- getUID
						configs <- readAllConfig
						isPass <- passDeadLine (configs !! 1)
						let twizzeName = (configs !! 2)
						inh <- openFile "twizze/names" ReadMode
						names <- readName
						hClose inh
						if (not True)
						then putStrLn "sorry, you are not registed in this class."
						else do
								if(not $  (head configs)  == "uploadTwizze")
								then putStrLn "sorry, you can not submit twizze now at current phase"
								else do
										if(isPass)
										then putStrLn "sorry, you can not submit homework because deadline is passed"
										else do
												twizzeContent <- readFile (head argus)
												appendFile ("../mango/twizze/" ++ twizzeName ++ "/" ++ uid ++ "/twizze.hs") ("--------------------------\n" ++ twizzeContent)
												setFileMode ("../mango/twizze/" ++ twizzeName ++ "/" ++ uid ++ "/twizze.hs") 0o755
												putStrLn "upload successfully"



{-
-- user need use this command to submit their twizzer homeworks
submittwizze :: String -> IO ()
submittwizze path = do
					uid <- getUID
					configs <- readAllConfig
					let twizzeName = configs !! 2
					copyFile path ("../mango/twizze/" ++ twizzeName ++ "/" ++ uid ++ "/" ++ twizzeName ++ ".hs")
					setFileMode ("../mango/twizze/" ++ twizzeName ++ "/" ++ uid ++ "/" ++ twizzeName ++ ".hs") 0o755
					putStrLn "submit homework successfully"
				--need to copy twizze file from student directory to administrator's directory
				


-}


-- 
--
--"26 Jan 2012 10:54 AM"
passDeadLine :: String -> IO Bool
passDeadLine dateString = do
							let deadline = readTime defaultTimeLocale "%d %b %Y %l:%M %p" dateString :: UTCTime
							currentTime <- getCurrentTime --IO UTCTime 
							let timeDiffInSeconds = floor (toRational (diffUTCTime deadline currentTime))
							if timeDiffInSeconds > 0
							then return False
							else return True






--call system function
getUID :: IO String
getUID = getEffectiveUserName

--whether current user is registed in this class
--need to be refactored
isRegistered :: String -> [String] -> Bool
isRegistered uid names = uid `elem` names

--check whether currentet state allows students to submit their twizzes.
--need to be refactored
--curState = "uploadTwizze"
{-
alreadySubmitted :: String -> Bool
alreadySubmitted uid = False
-}

--readTwizze
--
--
--
--




showBuddyTwizze :: IO ()
showBuddyTwizze = do
					configs <- readAllConfig
					let state = head configs
					let twizzeName = (configs !! 2)
					if (not (state == "submitReview"))
					then putStrLn "sorry, you can not see buddies homework at this stage"
					else do
							uid <- getUID
							if (not True)
							then putStrLn "sorry, you are not registred in this class"
							else do
									inh <- openFile "twizze/buddy" ReadMode
									listBuddies <- readBuddies inh []-- read all buddies from file buddy
									buddy <- findBuddy uid listBuddies -- look for current users buddy
									content <- readFile ("../mango/twizze/" ++ twizzeName ++ "/" ++ buddy ++ "/twizze.hs") -- read buddies's homework and print out
									putStrLn content
									hClose inh


{--
findBuddy :: String -> [(String, String)] -> String
findBuddy uid listBuddy = "meisi"


readBuddies :: IO [(String, String)]
readBuddies = do
				return [("qiq", "meisi")]

--}


findBuddy :: String -> [(String, String)] -> IO (String)
findBuddy uid [] = return  "can't find buddy"
findBuddy uid (x:listBuddy) = do
								if (uid == (fst x))
								then return (snd x)
								else do
									findBuddy uid listBuddy 


readBuddies :: Handle -> [(String, String)] -> IO [(String, String)]
readBuddies inh buddies = do
							ineof <- hIsEOF inh
							if ineof
							then return buddies
							else do
									line <- hGetLine inh
									let names = words line
									let buddy = parseBuddies names
									readBuddies inh (buddy ++ buddies)


parseBuddies :: [String] -> [(String, String)]
parseBuddies [] = []
parseBuddies (x:y:xs) = (x,y):(parseBuddies xs)




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



--}
-- this function is used to shuffle a list randomly
{-
shuffleList :: [a] -> IO [a]
shuffleList [i] = return [i]
shuffleList xs = do
					ran <- getRandom (length xs - 1)
					let front = (xs !! ran)
					list <- shuffleList (take ran xs ++ drop (ran + 1) xs)
					return (front : list)

-}

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
					putStrLn ("nextState:" ++ nextState)
					outh <- openFile "twizze/config" WriteMode
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


submitReview :: String -> IO ()
submitReview filePath = do
						uid <- getUID
						inh <- openFile "twizze/buddy" ReadMode
						listBuddies <- readBuddies inh []-- read all buddies from file buddy
						reviewWho <- findBuddy uid listBuddies
						hClose inh
						--isRegistered <- registered
						configs <- readAllConfig
						let twizzeName = (configs !! 2)
						let deadline = (configs !! 1)
						let state = (configs !! 0)
						isPass <- passDeadLine deadline
						if (not True)
						then putStrLn "sorry,"
						else do
								if( state /= "submitReview")
								then putStrLn "not correct phase"
								else do
										if(isPass)
										then putStrLn "sorry, pass deadline"
										else do
												reviewContent <- readFile filePath
												appendFile ("../mango/twizze/" ++ twizzeName ++ "/" ++ reviewWho ++ "/review") ("------------------------\n" ++ reviewContent)
												setFileMode ("../mango/twizze/" ++ twizzeName ++ "/" ++ reviewWho ++ "/review") 0o755








seeReview :: IO ()
seeReview = do
			uid <- getUID
			configs <- readAllConfig
			let twizzeName = (configs !! 2)
			let state = (configs !! 0)
			if( state /= "seeReview")
			then putStrLn "sorry, not correct phase"
			else do
					content <- readFile ("../mango/twizze/" ++ twizzeName ++ "/" ++ uid ++ "/" ++ "review")
					putStrLn content
