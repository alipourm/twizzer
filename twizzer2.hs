{-
 - CS 583 Functional Programming
 - Twizzer Project
 - Twizzer group 3
 - Group members: Qi Qi, Amin Alipour, Qingkai Lu
-}

 {-# LANGUAGE TypeFamilies , DataKinds, NamedFieldPuns #-}

import System.IO
import System.Directory
import System.Posix
import System.Exit
--import System.Posix.Files
import System.Locale
import System.Time
import System.Random  --Random module not found in Haskell 7.4.1
import Data.Time
import Data.Time.Format
import Data.Time.Clock







data UserType = Student | Instructor | NotRegistered 
              deriving (Show, Eq,Read)

data User = User {userId:: String,
                  userType :: UserType,
                  name:: String} 
          deriving (Show, Eq, Read)


data AssignmentType = Twiz | Review
                 deriving (Show, Eq, Read)
type File  = String

type Time = String



data Config = C Course [Assignment]
                  deriving (Show,Eq,Read)


getCourse :: Config -> Course
getCourse cfg = crs 
                where C crs _ = cfg

getAssignments ::Config -> [Assignment]
getAssignments cfg = ars
                     where C _ ars = cfg
c = Course{coursePath = "/home/amin/fun/twizzer", studentNameFile = "/home/amin/fun/twizzer/twizzer"}
a1 = Assignment {assignmentName = "Twiz1", assignmentType = Twiz, startTime = "26Jan2012-10:54AM",deadline="26Jul2013-10:54AM",description ="", points = 1 }
a2 = Assignment {assignmentName = "Twiz1", assignmentType = Review, startTime = "26Jan2012-10:54AM",deadline="26Jul2013-10:54AM",description ="", points = 1 }

cf = C c [a1,a2]
cfg :: IO Config
cfg = return cf

data Course = Course{ 
                     -- courseID   :: String,
                     -- cName      :: String,
                     -- term       :: String,
                     -- startDate  :: String,
                     -- finalDate  :: String,
                     coursePath :: String, 
                     studentNameFile ::String}
              deriving (Show, Eq, Read)

data Assignment = Assignment {assignmentName   :: String,
                              assignmentType :: AssignmentType,
                              startTime      :: Time,
                              deadline       :: Time,
                              description    :: String,
                              points :: Int} 
                deriving (Show, Eq, Read)
                     
data Submission = Submission  {sAssignmentID :: Int,
                               sUserID       :: Int,
                               content       :: String,
                               stime         :: Time}
                deriving (Show, Eq, Read)
                         
type Function = [String] -> IO ()


dummy::[String] -> IO ()
dummy _ = undefined

data Command = Command {cname :: String,
                        cUser :: UserType,
                        cDesc :: String,
                        cOp   :: Function, 
                        argc  :: Int
                        }
           --    deriving (Show, Eq)

usageSCmd   = Command {cname = "usage", cUser = Student, cDesc = "usage :  to give usage", cOp = printUsage, argc = 0}
subTCmd     = Command {cname = "subT", cUser = Student, cDesc = "subT filename  :  to give usage" , cOp = twizzeHandler, 
                          argc = 1}
subRCmd     = Command {cname = "subR", cUser = Student, cDesc = "subR  :  to submit review" , cOp = submitReview, 
                       argc = 1}
seeRCmd     = Command {cname = "seeR", cUser = Student, cDesc = "seeR  :  to see review" , cOp = seeReview, argc = 0}
showTCmd    = Command {cname = "showT", cUser = Student, cDesc = "showT  :  to see buddies twiz", cOp = showBuddyTwizze, argc = 0}
showACmd    = Command {cname = "seeA", cUser = Student, cDesc = "to see assignment", cOp = seeAssignment, argc = 0}
usageICmd   = Command {cname = "usage", cUser = Instructor, cDesc = "usage  :  to give usage", cOp = printUsage, argc = 0}
chgstateCmd = Command {cname = "addHW", cUser = Instructor, cDesc = "add HW  :  to add homework", cOp = setUpHandler, argc = 5}
setupCmd    = Command {cname = "setUpCourse", cUser = Instructor, cDesc = "setUpCourse (not implemented)  :  to set up a new course" , cOp = dummy,  argc = 0}
checkTCmd   = Command {cname = "checkT", cUser = Instructor, cDesc = "checkT  :  to check homework" , cOp = checkTwizze , argc = 0}
checkRCmd   = Command {cname = "checkR", cUser = Instructor, cDesc = "checkR  :  to check review" , cOp =  checkReview, argc = 0}
combineTCmd = Command {cname = "combineT", cUser = Instructor, cDesc = "combineT  :  to report all twizes" , cOp = combineAllTwizze, argc = 0}
combineRCmd = Command {cname = "combineR", cUser = Instructor, cDesc = "combineR  :  to report all reviews" , cOp = combineAllReview, argc = 0}

commands :: [Command] 
commands = [usageSCmd, subTCmd, subRCmd, seeRCmd, showTCmd, showACmd, usageICmd, chgstateCmd, setupCmd, checkTCmd, checkRCmd, combineTCmd, combineRCmd]






getUID :: IO String
getUID = getEffectiveUserName

isAdmin :: IO Bool
isAdmin = do
  uid <- getUID
  if uid == "amin" 
    then return True
    else return False



availablecommands ::  IO  [Command]
availablecommands  = do 
  l <- isAdmin
  if l then return  ( (filter  (\x -> (cUser x == Instructor)) commands))
    else return  ( (filter  (\x -> (cUser x == Student)) commands))


availableCmdName ::  IO  [String]
availableCmdName  = 
  availablecommands >>= return.(map cname)



seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as


printUsage :: [String] -> IO ()
printUsage _ = do
   cmdlist <- availablecommands   
   seqn (map (putStrLn.cDesc) cmdlist) 
   
   

main :: IO ()
main = do
        putStrLn "Type \"usage\" for Command Usage"
        line <- getLine
        let commands = words line
        execute commands
        main




getOp :: String -> IO [Command] -> IO  Function 
getOp cName cmds = do cmdList <- cmds
                      return (head [cOp x| x <-cmdList , cname x == cName])
                      
getArgNum :: String -> IO [Command] -> IO Int 
getArgNum cName cmds = do cmdList <- cmds
                          return (head [argc x| x <-cmdList , cname x == cName])


execute :: [String] -> IO ()
execute [] = putStrLn "Type \"usage\" for Command Usage"
execute (cmd:args) = do  l <- availableCmdName
                         case (elem cmd l) of  
                           True ->  do function <- getOp cmd availablecommands
                                       argnum <- getArgNum cmd availablecommands
                                       if length args < argnum 
                                         then putStrLn "Too few arguments"
                                         else function args            
                           False -> case cmd of
                                        "quit"    -> exitSuccess 
                                        otherwise -> putStrLn "Wrong command, please try again"
                                        




configFile::String
configFile = "cs583.cfg"

configs :: IO Config
configs = cfg
--configs = loadConfig configFile


loadConfig ::String ->  IO Config
loadConfig cfgFileName  = do content <- readFile cfgFileName 
                             let x = (read content) :: Config          
                             return x




passDeadLine :: String -> UTCTime ->  Bool
passDeadLine dateString curTime  = do
                           let deadline = readTime defaultTimeLocale "%d%b%Y-%l:%M%p" dateString :: UTCTime
                           let timeDiffInSeconds = floor (toRational (diffUTCTime deadline curTime))
                           if (timeDiffInSeconds + 25200) > 0
                             then  False
                             else  True

                             
openAssignments :: IO [Assignment]
openAssignments = do c <- configs
                     let as = getAssignments c
                     currentTime <- getCurrentTime --IO UTCTime
                     let k  = filter (\a -> not (passDeadLine (deadline a) currentTime)) as
                     let k' = filter (\a ->  (passDeadLine  (startTime a) currentTime) ) k
                     return k

isTwizPhase :: IO Bool
isTwizPhase = do as <- openAssignments
                 return (or (map (\c -> assignmentType c == Twiz ) as))
isReviewPhase :: IO Bool
isReviewPhase = do as <- openAssignments
                   return (or (map (\c -> assignmentType c == Review ) as))






setUpHandler :: [String] -> IO ()
setUpHandler args = do
  createConfig args --create configuration file, every setup will generate a new config file
  createProject
  names <- readName --read all students names from file, and store names in varibal names, which is a list of string
  buildBuddies names



{--
data Assignment = Assignment {assignmentID  :: Int,
                              assignmentType :: AssignmentType,
                              deadline       :: Time,
                              description    :: String,
                              points :: Int} 
                deriving (Show, Eq, Read)
   
--}


  



--first argument is deadline
--second argument is twizze name
createConfig :: [String] -> IO ()
createConfig args = do
                        workingDir <- getWorkingDirectory
                        let newAssignment = Assignment {    
                              assignmentName = (args !! 0), 
                              assignmentType = if (args !! 1) == "Twiz" then Twiz
                                               else  Review,
                              startTime = (args!! 2),
                              deadline = (args !! 3),
                              description = (args !! 4),
                              points = read (args !! 5) :: Int}
                                                                      
                        cfg <- configs
                        let as = getAssignments cfg
                        let crs = getCourse cfg
                        let newConfig = C crs (newAssignment:as) 
                        outh <- openFile configFile WriteMode              
                        hPutStrLn outh $ show newConfig
                        hClose outh
                        setFileMode (configFile) 0o664
                        setFileMode ("names") 0o664






createDirs :: [Assignment] -> IO ()
createDirs [] = return ()
createDirs (x:xs) = do let aName = assignmentName x
                       b <- System.Directory.doesDirectoryExist aName
                       if not b then 
                         do
                          System.Directory.createDirectory aName -- create folder for current project
                          setFileMode aName 0o751  
                          names <- readName
                          createFolders (aName ++ "/") names 0o753
                          createDirs xs     
                       else
                          createDirs xs
                  


createProject :: IO ()
createProject = do
                  cfg <- configs
                  let as = getAssignments cfg
                  createDirs as


createFolders :: String -> [String] -> FileMode -> IO ()
createFolders _ [] _ = return ()
createFolders path (x:xs) mode = do
                              System.Directory.createDirectory (path ++ x)
                              setFileMode (path ++ x) mode
                              createFolders path xs mode


buildBuddies :: [String] -> IO ()
buildBuddies names = do
                       outh <- openFile "buddy" WriteMode
                       ran <- getRandom 60
                       let randomNames = randPerm (mkStdGen ran) names
                       let randomNamesShiftByOne = (tail randomNames) ++ [(head randomNames)]
                       writeBuddies outh randomNamesShiftByOne randomNames
                       hClose outh
                       setFileMode ("buddy") 0o664





randPerm :: StdGen -> [a] -> [a]
randPerm _ [] = []
randPerm gen xs = let (n, newGen) = randomR (0, length xs - 1) gen
                      front = xs !! n
                  in front : randPerm newGen (take n xs ++ drop (n+1) xs)






readName :: IO [String]
readName = do
             cfg <- configs
             let crs = getCourse cfg
             let path = (coursePath crs)
             content <- readFile (path ++ "/names")
             return . words $ content


-- write buddy information into a file
writeBuddies :: Handle -> [String] -> [String]  -> IO ()
writeBuddies outh [] _ = return ()
writeBuddies outh _ [] = return ()
writeBuddies outh (n:names)(sn:shuffledN) = do
    hPutStrLn outh ( n ++ " " ++ sn )
    --hPutStrLn outh (sn)
    writeBuddies outh names shuffledN


passDeadLine1 :: String -> IO Bool
passDeadLine1 dateString = do
                            let deadline = readTime defaultTimeLocale "%d%b%Y-%l:%M%p" dateString :: UTCTime
                            currentTime <- getCurrentTime --IO UTCTime
                            let timeDiffInSeconds = floor (toRational (diffUTCTime deadline currentTime))
                            if (timeDiffInSeconds + 25200) > 0
                            then return False
                            else return True



--client submit twizze, argu1 should be path
twizzeHandler :: [String] -> IO ()
twizzeHandler argus = undefined
{--  do cfg <- configs
                         let crs = getCourse cfg
                         let as  = getAssignments cfg
                         isInClass <- isRegistered
                         udefined
 --}                        




--whether current user is registed in this class
isRegistered :: IO Bool
isRegistered = do
                 uid <- getUID
                 names <- readName
                 return (uid `elem` names)


--



type Pos = (Int,Int)

getCh :: IO Char
getCh =  do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c
            

{-- un-implemented GUI 
cls :: IO ()
cls =  putStr "\ESC[2J"
goto :: Pos -> IO ()
goto (x,y) =  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
writeat :: Pos -> String -> IO ()
writeat p xs =  do goto p
                   putStrLn xs





processCh:: IO Char -> IO ()
processCh ch = do c <- ch
                  
showMenu :: IO()
showMenu = do as <- openAssignments
              putStr $ show (length as) 
              
           -- seqn [putStrLn  x | (y,xs) <- zip [1..(length as)] as , let x = assignmentName xs]
              cls
              putStr "\x1b[31m"
              System.Console.ANSI.Color Red
              seqn [writeat (2,y) x | (y,xs) <- zip [1..(length as)] as , let x = assignmentName xs]
           

--}





showBuddyTwizze :: [String]-> IO ()
showBuddyTwizze _ = do
                    configs <- readAllConfig
                    isInClass <- isRegistered
                    if(not isInClass)
                    then putStrLn "sorry, you are not registed in this class."
                    else do
                           let currentState = (head configs)
                           if (currentState /= "submitReview")
                           then putStrLn "sorry, you can not see buddies homework at current phase"
                           else do
                                  let twizzeName = (configs !! 2)
                                  let path = (configs !! 3)
                                  uid <- getUID
                                  inh <- openFile (path ++ "/buddy") ReadMode
                                  listBuddies <- readBuddies inh []-- read all buddies from file buddy
                                  buddy <- findBuddy uid listBuddies -- look for current users buddy
                                  hasFile <- doesFileExist (path ++ "/" ++ twizzeName ++ "/" ++ buddy ++ "/twizze.hs")
                                  if (not hasFile)
                                  then putStrLn "not homework found."
                                  else do
                                         content <- readFile (path ++ "/" ++ twizzeName ++ "/" ++ buddy ++ "/twizze.hs") -- read buddies's homework and print out
                                         putStrLn content
                                         hClose inh



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
                          outh <- openFile "config" WriteMode
                          if nextState == "submitReview"
                          then do
                                 hPutStrLn outh nextState
                                 hPutStrLn outh (argus !! 0)
                                 hPutStrLn outh (configs !! 2)
                                 hPutStrLn outh (configs !! 3)
                          else do
                                 hPutStrLn outh nextState
                                 hPutStrLn outh "none"
                                 hPutStrLn outh (configs !! 2)
                                 hPutStrLn outh (configs !! 3)
                          hClose outh



--how to find configuration file directory
readAllConfig :: IO [String]
readAllConfig = do
                  inh <- openFile "/nfs/stak/students/q/qiq/config" ReadMode
                  state <- hGetLine inh
                  deadline <- hGetLine inh
                  twizzeName <- hGetLine inh
                  workingDir <- hGetLine inh
                  hClose inh
                  return [state, deadline, twizzeName, workingDir]


submitReview :: [String] -> IO ()
submitReview (filePath:[]) = do
                          configs <- readAllConfig
                          isInClass <- isRegistered
                          if (not isInClass)
                          then putStrLn "sorry, you are not registered in this class."
                          else do
                                 let currentState = (configs !! 0)
                                 if( currentState /= "submitReview")
                                 then putStrLn "sorry, you can not submit review at current phase."
                                 else do
                                        let deadLine = (configs !! 1)
                                        isPass <- passDeadLine1 deadLine
                                        if(isPass)
                                        then putStrLn "sorry, you have passed deadline."
                                        else do
                                               uid <- getUID
                                               let path = (configs !! 3)
                                               let twizzeName = (configs !! 2)
                                               inh <- openFile (path ++ "/buddy") ReadMode
                                               listBuddies <- readBuddies inh []-- read all buddies from file buddy
                                               reviewWho <- findBuddy uid listBuddies
                                               hClose inh
                                               reviewContent <- readFile filePath
                                               appendFile (path ++ "/" ++ twizzeName ++ "/" ++ reviewWho ++ "/review") ("---review for " ++ reviewWho ++ " from " ++ uid ++ "------------\n" ++ reviewContent)
                                               setFileMode (path ++ "/" ++ twizzeName ++ "/" ++ reviewWho ++ "/review") 0o755



seeReview ::[String]-> IO ()
seeReview _ = do
              configs <- readAllConfig
              isInClass <- isRegistered
              if (not isInClass)
              then putStrLn "sorry, you are not registered in this class."
              else do
                     let currentState = (configs !! 0)
                     if( currentState /= "seeReview")
                     then putStrLn "sorry, you can not see review at current phase."
                     else do
                            uid <- getUID
                            let twizzeName = (configs !! 2)
                            let path = (configs !! 3)
                            hasFile <- doesFileExist (path ++ "/" ++ twizzeName ++ "/" ++ uid ++ "/review")
                            if (not hasFile)
                            then putStrLn "no review found."
                            else do
                                   review <- readFile (path ++ "/" ++ twizzeName ++ "/" ++ uid ++ "/review")
                                   putStrLn review




checkTwizze ::[String]-> IO ()
checkTwizze _ = do
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

checkStudent :: [String] -> IO ([String], [String])
checkStudent [] = return ([], [])
checkStudent (x:xs) = do
                        configs <- readAllConfig
                        let twizzeName = (configs !! 2)
                        --allNames <- readName
                        isTwizzeExist <- doesFileExist (twizzeName ++ "/" ++ x ++ "/twizze.hs")
                        rest <- checkStudent xs
                        if (isTwizzeExist)
                        then return (x:(fst rest), (snd rest))
                        else return ((fst rest), x:(snd rest))



checkReview :: [String] ->IO ()
checkReview _ = do
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

checkStudent_ :: [String] -> IO ([String], [String])
checkStudent_ [] = return ([], [])
checkStudent_ (x:xs) = do
                        configs <- readAllConfig
                        let twizzeName = (configs !! 2)
                        --allNames <- readName
                        isTwizzeExist <- doesFileExist (twizzeName ++ "/" ++ x ++ "/review")
                        rest <- checkStudent_ xs
                        inh <- openFile ((configs !! 3) ++ "/buddy") ReadMode
                        listBuddies <- readBuddies inh []
                        reviewer <- findReviewee x listBuddies
                        if (isTwizzeExist)
                        then return (reviewer:(fst rest), (snd rest))
                        else return ((fst rest), reviewer:(snd rest))




combineAllTwizze :: [String] -> IO ()
combineAllTwizze s = do
                     configs <- readAllConfig
                     let path = (configs !! 2) ++ "/"
                     outh <- openFile (path ++ "allTwizze") WriteMode
                     names <- readName
                     combineTwizze outh path names
                     hClose outh


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


combineAllReview ::[String] -> IO ()
combineAllReview _ = do
                     configs <- readAllConfig
                     let path = (configs !! 2) ++ "/"
                     outh <- openFile (path ++ "allReview") WriteMode
                     names <- readName
                     combineReview outh path names
                     hClose outh




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

seeAssignment :: [String] -> IO ()
seeAssignment _ = do
                  configs <- readAllConfig
                  let path = (configs !! 3)
                  let twizzeName = (configs !! 2)
                  content <- readFile (path ++ "/" ++ twizzeName ++ "/assignment")
                  putStrLn content
