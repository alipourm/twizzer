{-
 - CS 583 Functional Programming
 - Twizzer Project
 - Twizzer group 3
 - Group members: Qi Qi, Amin Alipour, Qingkai Lu
-}

import System.IO
import System.Directory
import System.Posix
import System.Locale
import System.Time
import System.Random  --Random module not found in Haskell 7.4.1
import Data.Time
import Data.Time.Format
import Data.Time.Clock
import System.Exit


configFile::String
configFile = "cs583.cfg"

rosterFile :: String
rosterFile = "cs583.names"

buddyFile :: String
buddyFile = "cs583.buddy"

data Command = Command {cname :: String,
                        cUser :: UserType,
                        cOp   :: Function,
                        argc  :: Int
                        }


data UserType = Student | Instructor | NotRegistered
                deriving (Show, Eq)


type Function = [String] -> IO ()

data State = UploadTwizze | SubmitReview | SeeReview
             deriving (Show, Eq)

data Assignment = Assignment {
                              assignmentName :: String,
                              deadlineT      :: UTCTime,
                              deadlineR      :: UTCTime,
                              filepath       :: String
                              }
                deriving (Show, Eq, Read)




--Student's commands
usageSCmd   = Command {cname = "usage", cUser = Student,  cOp = printUsage,   argc = 0}
subTCmd     = Command {cname = "subT",  cUser = Student,  cOp = subTHandler,  argc = 1}
subRCmd     = Command {cname = "subR",  cUser = Student,  cOp = subRHandler,  argc = 1}
seeRCmd     = Command {cname = "seeR",  cUser = Student,  cOp = seeRHandler,  argc = 0}
showTCmd    = Command {cname = "showT", cUser = Student,  cOp = showTHandler, argc = 0}
showACmd    = Command {cname = "showA", cUser = Student,  cOp = showAHandler, argc = 0}

--Instructor's commands
usageICmd   = Command {cname = "usage",    cUser = Instructor,  cOp = printUsage,      argc = 0}
setupCmd    = Command {cname = "setUp",    cUser = Instructor,  cOp = setUpHandler,    argc = 3}
checkTCmd   = Command {cname = "checkT",   cUser = Instructor,  cOp = checkTHandler,   argc = 0}
checkRCmd   = Command {cname = "checkR",   cUser = Instructor,  cOp = checkRHandler,   argc = 0}
combineTCmd = Command {cname = "combineT", cUser = Instructor,  cOp = combineTHandler, argc = 0}
combineRCmd = Command {cname = "combineR", cUser = Instructor,  cOp = combineRHandler, argc = 0}
checkS      = Command {cname = "checkS",   cUser = Instructor,  cOp = checkSHandler,   argc = 0}

commands :: [Command]
commands = [checkS, usageSCmd, subTCmd, subRCmd, seeRCmd, showTCmd, showACmd, usageICmd, setupCmd, checkTCmd, checkRCmd, combineTCmd, combineRCmd]



main :: IO ()
main = do
         putStrLn "type \"usage\" for Command Usage"
         line <- getLine
         let cmds = words line
         execute cmds
         main


printUsage :: Function
printUsage argv = do
               hasFile <- doesFileExist rosterFile
               if(not hasFile)
               then printUsageForUser
               else printUsageForAdmin


whatStateNow :: IO State
whatStateNow = do
                 configs <- readAllConfig
                 let twizzeDeadline = deadlineT configs
                 let reviewDeadline = deadlineR configs
                 currentTime <- getCurrentTime
                 let diff1 = (floor (toRational (diffUTCTime twizzeDeadline currentTime)) + 25200)
                 let diff2 = (floor (toRational (diffUTCTime currentTime reviewDeadline)) - 25200)
                 if (diff1 > 0)
                 then return UploadTwizze
                 else do
                        if (diff2 > 0)
                        then return SeeReview
                        else do
                               return SubmitReview



availableCommands :: IO [Command]
availableCommands = do
                      who <- whoAreyou
                      case who of
                          Instructor -> return (filter (\cmd -> cUser cmd == Instructor) commands)
                          Student -> return (filter (\cmd -> cUser cmd == Student) commands)




availableCmdName :: IO [String]
availableCmdName = availableCommands >>= return.(map cname)



getOp :: String -> IO [Command] -> IO Function
getOp cName cmds = do
                     cmdList <- cmds
                     return (head [cOp x| x <-cmdList , cname x == cName])


getArgNum :: String -> IO [Command] -> IO Int
getArgNum cName cmds = do
                         cmdList <- cmds
                         return (head [argc x| x <-cmdList , cname x == cName])



execute :: [String] -> IO ()
execute [] = putStrLn "Type \"usage\" for Command Usage"
execute all@(cmd:args) = do
                           cmds <- availableCmdName
                           case (cmd `elem` cmds) of
                               True ->  do
                                          function <- getOp cmd availableCommands
                                          argnum <- getArgNum cmd availableCommands
                                          if (length args /= argnum)
                                          then putStrLn "incorrect arguments"
                                          else function args
                               False -> case cmd of
                                            "exit"    -> exitSuccess
                                            otherwise -> putStrLn "Wrong command, please try again"


buildBuddies :: [String] -> IO ()
buildBuddies names = do
                       outh <- openFile buddyFile WriteMode
                       ran <- getRandom 60
                       let randomNames = randPerm (mkStdGen ran) names
                       let randomNamesShiftByOne = (tail randomNames) ++ [(head randomNames)]
                       writeBuddies outh randomNamesShiftByOne randomNames
                       hClose outh
                       setFileMode (buddyFile) 0o664




whoAreyou :: IO (UserType)
whoAreyou = do
            isadmin <- isAdmin
            case isadmin of
                True -> return Instructor
                False -> return Student



randPerm :: StdGen -> [a] -> [a]
randPerm _ [] = []
randPerm gen xs = let (n, newGen) = randomR (0, length xs - 1) gen
                      front = xs !! n
                  in front : randPerm newGen (take n xs ++ drop (n+1) xs)



--first argument is deadline for twizze
--second argument is deadline for review
--third argument is assignment name
createConfig :: [String] -> IO ()
createConfig argv = do
                        workingDir <- getWorkingDirectory
                        let deadline1 = readTime defaultTimeLocale "%d%b%Y-%l:%M%p" (argv !! 0) :: UTCTime
                        let deadline2 = readTime defaultTimeLocale "%d%b%Y-%l:%M%p" (argv !! 1) :: UTCTime
                        let config = Assignment {assignmentName = argv !! 2, deadlineT = deadline1, deadlineR = deadline2, filepath = workingDir}
                        outh <- openFile configFile WriteMode
                        hPutStrLn outh (show config) -- working directory
                        hClose outh
                        setFileMode (configFile) 0o664
                        setFileMode (rosterFile) 0o664


readName :: IO [String]
readName = do
             configs <- readAllConfig
             let path = (filepath configs)
             content <- readFile (path ++ "/" ++ rosterFile)
             return . words $ content


-- write buddy information into a file
writeBuddies :: Handle -> [String] -> [String]  -> IO ()
writeBuddies outh [] _ = return ()
writeBuddies outh _ [] = return ()
writeBuddies outh (n:names)(sn:shuffledN) = do
								hPutStrLn outh ( n ++ " " ++ sn )
								--hPutStrLn outh (sn)
								writeBuddies outh names shuffledN







--admin function: setup twizze project
setUpHandler :: Function
setUpHandler argv = do
                       createConfig argv --create configuration file, every setup will generate a new config file
                       createProject
                       names <- readName --read all students names from file, and store names in varibal names, which is a list of string
                       buildBuddies names


--admin function: check who has submited the twizze
--no argument
checkTHandler :: Function
checkTHandler argv = do
                    isadmin <- isAdmin
                    if (not isadmin)
                    then putStrLn "sorry you are not allowed to take this step"
                    else do
                        names <- readName
                        students <- checkStudent names
                        putStrLn "who has turned in the twizze:"
                        putStrLn . show $ (fst students)
                        putStrLn "who has not turned in the twizze:"
                        putStrLn . show $ (snd students)



--admin function: check who has submited the review
--no argument
checkRHandler :: Function
checkRHandler argv = do
                        isadmin <- isAdmin
                        if (not isadmin)
                        then putStrLn "sorry you are not allowed to take this step"
                        else do
                            names <- readName
                            students <- checkStudent_ names
                            putStrLn "who has turned in the review:"
                            putStrLn . show $ (fst students)
                            putStrLn "who has not turned in the review:"
                            putStrLn . show $ (snd students)



--admin function: combine all twizzes
--no argument
combineTHandler :: Function
combineTHandler argv = do
                     configs <- readAllConfig
                     let path = (filepath configs) ++ "/" ++ (assignmentName configs) ++ "/"
                     outh <- openFile (path ++ "allTwizze") WriteMode
                     names <- readName
                     combineTwizze outh path names
                     hClose outh


checkSHandler :: Function
checkSHandler argv = do
                       stt <- whatStateNow
                       putStrLn (show stt)


--admin function: combine all reviews
--no argument
combineRHandler :: Function
combineRHandler argv = do
                            configs <- readAllConfig
                            let path = (filepath configs) ++ "/" ++ (assignmentName configs) ++ "/"
                            outh <- openFile (path ++ "allReview") WriteMode
                            names <- readName
                            combineReview outh path names
                            hClose outh






createProject :: IO ()
createProject = do
                  configs <- readAllConfig
                  System.Directory.createDirectory (assignmentName configs)-- create folder for current project
                  setFileMode (assignmentName configs) 0o751
                  names <- readName
                  createFolders ((assignmentName configs) ++ "/") names 0o753


createFolders :: String -> [String] -> FileMode -> IO ()
createFolders _ [] _ = return ()
createFolders path (x:xs) mode = do
                              System.Directory.createDirectory (path ++ x)
                              setFileMode (path ++ x) mode
                              createFolders path xs mode




--client function: submit twizze
--one argument: filePath
subTHandler :: Function
subTHandler argv = do
                    configs <- readAllConfig
                    isInClass <- isRegistered
                    if (not isInClass)
                    then putStrLn "sorry, you can not submit twizze because you are not registed in this class."
                    else do
                            currentState <- whatStateNow
                            if(currentState /= UploadTwizze)
                            then putStrLn "sorry, you can not submit twizze because deadline is passed"
                            else do
                                    uid <- getUID
                                    let twizzeName = (assignmentName configs)
                                    let path = (filepath configs)
                                    twizzeContent <- readFile (head argv)
                                    appendFile (path ++ "/" ++ twizzeName ++ "/" ++ uid ++ "/twizze.hs") ("---homework from " ++ uid ++ "------------------\n" ++ twizzeContent)
                                    setFileMode (path ++ "/" ++ twizzeName ++ "/" ++ uid ++ "/twizze.hs") 0o755
                                    putStrLn "upload successfully"



-- client function: submit review
-- one argument: filePath
subRHandler :: Function
subRHandler argv = do
                    configs <- readAllConfig
                    isInClass <- isRegistered
                    if (not isInClass)
                    then putStrLn "sorry, you are not registered in this class."
                    else do
                        currentState <- whatStateNow
                        if( currentState /= SubmitReview)
                        then putStrLn "sorry, you can not submit review at current phase."
                        else do
                                uid <- getUID
                                let path = (filepath configs)
                                let twizzeName = (assignmentName configs)
                                inh <- openFile (path ++ "/" ++ buddyFile) ReadMode
                                listBuddies <- readBuddies inh []-- read all buddies from file buddy
                                reviewWho <- findBuddy uid listBuddies
                                hClose inh
                                reviewContent <- readFile (head argv)
                                appendFile (path ++ "/" ++ twizzeName ++ "/" ++ reviewWho ++ "/review") ("---review for " ++ reviewWho ++ " from " ++ uid ++ "------------\n" ++ reviewContent)
                                setFileMode (path ++ "/" ++ twizzeName ++ "/" ++ reviewWho ++ "/review") 0o755



--client function: see reviews written by user's buddy
--no argument
seeRHandler :: Function
seeRHandler argv = do
                    configs <- readAllConfig
                    isInClass <- isRegistered
                    if (not isInClass)
                    then putStrLn "sorry, you are not registered in this class."
                    else do
                            currentState <- whatStateNow
                            if( currentState /= SeeReview)
                            then putStrLn "sorry, you can not see review at current phase."
                            else do
                                    uid <- getUID
                                    let twizzeName = (assignmentName configs)
                                    let path = (filepath configs)
                                    hasFile <- doesFileExist (path ++ "/" ++ twizzeName ++ "/" ++ uid ++ "/review")
                                    if (not hasFile)
                                    then putStrLn "no review found."
                                    else do
                                        review <- readFile (path ++ "/" ++ twizzeName ++ "/" ++ uid ++ "/review")
                                        putStrLn review



--client function: show buddy's twizze
--no argument
showTHandler :: Function
showTHandler argv = do
                    configs <- readAllConfig
                    isInClass <- isRegistered
                    if(not isInClass)
                    then putStrLn "sorry, you are not registed in this class."
                    else do
                        currentState <- whatStateNow
                        if (currentState /= SubmitReview)
                        then putStrLn "sorry, you can not see buddies homework at current phase"
                        else do
                                let twizzeName = (assignmentName configs)
                                let path = (filepath configs)
                                uid <- getUID
                                inh <- openFile (path ++ "/" ++ buddyFile) ReadMode
                                listBuddies <- readBuddies inh []-- read all buddies from file buddy
                                buddy <- findBuddy uid listBuddies -- look for current users buddy
                                hasFile <- doesFileExist (path ++ "/" ++ twizzeName ++ "/" ++ buddy ++ "/twizze.hs")
                                if (not hasFile)
                                then putStrLn "not homework found."
                                else do
                                        content <- readFile (path ++ "/" ++ twizzeName ++ "/" ++ buddy ++ "/twizze.hs") -- read buddies's homework and print out
                                        putStrLn content
                                        hClose inh


--client function: show assignment
--no argument
showAHandler :: Function
showAHandler argv = do
                    configs <- readAllConfig
                    let path = (filepath configs)
                    let twizzeName = (assignmentName configs)
                    content <- readFile (path ++ "/" ++ twizzeName ++ "/assignment")
                    putStrLn content



--whether current user is registed in this class
isRegistered :: IO Bool
isRegistered = do
                 uid <- getUID
                 names <- readName
                 return (uid `elem` names)


--
--"26Jan2012-10:54AM"
passDeadLine :: String -> IO Bool
passDeadLine dateString = do
                            let deadline = readTime defaultTimeLocale "%d%b%Y-%l:%M%p" dateString :: UTCTime
                            currentTime <- getCurrentTime --IO UTCTime
                            let timeDiffInSeconds = floor (toRational (diffUTCTime deadline currentTime))
                            if (timeDiffInSeconds + 25200) > 0
                            then return False
                            else return True



--call system function
getUID :: IO String
getUID = getEffectiveUserName


isAdmin :: IO Bool
isAdmin = do
            hasFile <- doesFileExist (rosterFile)
            if (not hasFile)
            then return False
            else return True



findBuddy :: String -> [(String, String)] -> IO (String)
findBuddy uid [] = error "can not find buddy"
findBuddy uid (x:listBuddy) = do
								if (uid == (fst x))
								then return (snd x)
								else do
									findBuddy uid listBuddy



findReviewee :: String -> [(String, String)] -> IO (String)
findReviewee uid [] = error "can not find reviewer"
findReviewee uid (x:listBuddy) = do
                                   if (uid == (snd x))
                                   then return (fst x)
                                   else do
                                          findReviewee uid listBuddy



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


getRandom :: Int -> IO Int
getRandom range = do
			time <- getCurrentTime
			let ran = (floor . toRational . utctDayTime $ time) `mod` range
			return ran


--how to find configuration file directory
readAllConfig :: IO Assignment
readAllConfig = do
                  content <- readFile ("/nfs/stak/students/q/qiq/twizze/" ++ configFile)
                  let configs = (read content) :: Assignment
                  return configs


checkStudent :: [String] -> IO ([String], [String])
checkStudent [] = return ([], [])
checkStudent (x:xs) = do
                        configs <- readAllConfig
                        let twizzeName = (assignmentName configs)
                        --allNames <- readName
                        isTwizzeExist <- doesFileExist (twizzeName ++ "/" ++ x ++ "/twizze.hs")
                        rest <- checkStudent xs
                        if (isTwizzeExist)
                        then return (x:(fst rest), (snd rest))
                        else return ((fst rest), x:(snd rest))

checkStudent_ :: [String] -> IO ([String], [String])
checkStudent_ [] = return ([], [])
checkStudent_ (x:xs) = do
                        configs <- readAllConfig
                        let twizzeName = (assignmentName configs)
                        --allNames <- readName
                        isTwizzeExist <- doesFileExist (twizzeName ++ "/" ++ x ++ "/review")
                        rest <- checkStudent_ xs
                        inh <- openFile ((filepath configs) ++ "/" ++ buddyFile) ReadMode
                        listBuddies <- readBuddies inh []
                        reviewer <- findReviewee x listBuddies
                        if (isTwizzeExist)
                        then return (reviewer:(fst rest), (snd rest))
                        else return ((fst rest), reviewer:(snd rest))



combineTwizze :: Handle -> String -> [String] -> IO ()
combineTwizze _ _ [] = return ()
combineTwizze outh path (x:xs) = do
                              hasFile <- doesFileExist (path ++ x ++ "/twizze.hs")
                              if (not hasFile)
                              then combineTwizze outh path xs
                              else do
                                     content <- readFile (path ++ x ++ "/twizze.hs")
                                     hPutStrLn outh content
                                     combineTwizze outh path xs



combineReview :: Handle -> String -> [String] -> IO ()
combineReview _ _ [] = return ()
combineReview outh path (x:xs) = do
                              hasFile <- doesFileExist (path ++ x ++ "/review")
                              if (not hasFile)
                              then combineReview outh path xs
                              else do
                                     content <- readFile (path ++ x ++ "/review")
                                     hPutStrLn outh content
                                     combineReview outh path xs

printUsageForUser :: IO ()
printUsageForUser = do
                       curState <- whatStateNow                                  
                       case curState of                                          
                           UploadTwizze -> do                                    
                                             putStrLn "* current state: uploadTwizze"
                                             putStrLn "* usage: "                
                                             putStrLn "* Submit twizze:      subT filePath"
                                             putStrLn "* See assignment:     showA         "
                           SubmitReview -> do                                    
                                             putStrLn "* current state: submitReview"
                                             putStrLn "* usage: "                
                                             putStrLn "* See buddies twizze: showT         "
                                             putStrLn "* Submit review:      subR filePath "
                                             putStrLn "* See assignment:     showA         "
                           SeeReview -> do                                       
                                             putStrLn "* current state: seeReview"
                                             putStrLn "* See review:         seeR          "



 

printUsageForAdmin :: IO ()
printUsageForAdmin = do
                       putStrLn "****************************************************************************"
                       putStrLn "* Usage:                             cmd [argus]                           *"
                       putStrLn "* Set up homework:                   setUp deadlineT deadlineR twizzeNames *"
                       putStrLn "* Check who has submit homeworkd:    checkT                                *"
                       putStrLn "* Check who has submit review:       checkR                                *"
                       putStrLn "* Combine all tiwzzes:               combineT                              *"
                       putStrLn "* Combine all reviews:               combineR                              *"
                       putStrLn "****************************************************************************"


