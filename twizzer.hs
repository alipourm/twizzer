{-
 - CS 583 Functional Programming
 - Twizzer Project
 - Twizzer group 3
 - Group members: Qi Qi, Amin Alipour, Qingkai Lu
-}

import System.IO
import System.Posix
import System.Locale
import System.Time
import Data.Time
import Data.Time.Format
import Data.Time.Clock
import System.Random

data Handler = 

main :: IO ()
main = do
		line <- getLine
		let commands = parseCommand line
		execute commands
		main


execute :: [String] -> IO ()
execute all@(command:argus)
				| command == "setup" = setUpHandler argus -- admin 
				| command == "nextStep" = -- admin
				| command == "showTwizze" = showTwizze-- user run this command after admin has set up the twizze homework
				| command == "submitTwizze" = twizzeHandler argus -- user wants to submit their solution
				| command == "submitReview" = reviewHandler argus -- user
				| otherwise = putStrLn "shit !!!"


--administrator sets up twizzer system
--arguments should follow: deadline randomSeed twizzefileName
--there should be a configuration file called "config.txt", which stores dealline for current twizze, and students names.
--fisrt, setUpHandler reads names from config.txt, and assigns buddies for each student, and store buddies information in another file,
--called buddy.txt
--
--
setUpHandler :: [String] -> IO ()
setUpHandler argus = do
						inh <- openFile "config.txt" ReadMode
						outh <- openFile "buddy.txt" WriteMode
						names <- readName inh [] --read all students names from file, and store names in varibal names, which is a list of string
						let randomNames = shuffleList (mkStdGen 33) names -- shuffle name list, making it randomly.
						_ <- writeBuddies outh randomNames -- write those shuffled names into a file.
						hClose inh
						hClose outh


--readName function reads all students names from input handler, it recursively call itself.
readName :: Handler -> [String] -> IO [String]
readName inh names = do
				ineof <- hIsEOF inh
				if ineof
				then return names
				else do name <- hGenLine inh
						readName inh name:names
				

-- write buddy information into a file
writeBuddies :: Handler -> [String] -> pos -> IO ()
writeBuddies outh [] _ = return ()
writeBuddies outh (n:names) i = do
								hPutStrLn outh n ++ " " 
								writeBuddies outh names

writeLoop :: Handler -> Handler -> IO ()
writeLoop inh outh = do
						ineof <- hIsEOF inh
						if ineof
						then return ()
						else do intStr <- hGenLine inh
								hPutStrLn outh 
						

--client submit twizze, argu1 should be path
twizzeHandler :: [String] -> IO ()
twizzeHandler argus = do
						uid <- getUID
						if (not (isRegisted uid))
						then putStrLn "sorry, you are not registed in this class."
						else do
								if(not curState == Task)
								then putStrLn "sorry, you can not submit twizze now."
								else do
										if(not containFolder(uid))
										then --create folder
										else do
												if(invalidPath . head $ argus)
												then putStrLn "sorry, invalid path."
												else do
														if(alreadySubmitted())
														then --change old twizze's name to twizzeX_old.hs
														else do
																submittwizze (head agus)
																putStrLn "upload successfully"


reviewHandler :: [String] -> IO ()
reviewHandler argus = do

--xxxHandler :: IO ()

-- check whether there is already a folder for this user under admin's directory
containFolder :: String -> Bool
containFolder uid = 

-- check whether path for twizze are valid
invalidPath :: String -> Bool
invalidPath path = --

parseCommand :: String -> [String]
parseCommand commandLine = 

--call system function
getUID :: IO String
getUID = getEffectiveUserName

--whether current user is registed in this class
isRegisted :: String -> Bool
isRegisted uid = 


showTwizze :: IO ()
showTwizze = do
			if (not isRegisted(getUID))
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



-- this function is used to shuffle a list randomly
shuffleList :: StdGen -> [a] -> [a]
shuffleList _ []   = []
shuffleList gen xs = let (n,newGen) = randomR (0,length xs -1) gen
						front = xs !! n
						in  front : randPerm newGen (take n xs ++ drop (n+1) xs)

-- user need use this command to submit their twizzer homeworks
submittwizze :: String -> IO ()
submittwizze path = do
				--need to copy twizze file from student directory to administrator's directory
