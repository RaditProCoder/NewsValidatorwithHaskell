module Main where

-- Import Dependency Library
import Data.List
import Data.Time
import Data.Time.Format
import qualified Data.Char as Foo
import Data.Time.Clock
import Data.Time.Calendar
import System.Console.ANSI

-- Data Type Setup Configuration
type NewsMainDatabase = (AuthorName, InfoType,HeadLines,NewsCat)
type InfoLogs = (AuthorName,HeadLines,SystemDate)
type ObservationRegistered = (AuthorName,SystemDate,DateReg)
type ObservationRegisteredByName = (AuthorName,HeadLines,InfoPred,NewsCat,InfoType)
type NewsRecommender = (AuthorName,InfoType,InfoPred)
type AuthorName = String
type InfoType = String
type HeadLines = [String]
type HeadLines2 = String
type NewsCat = String
type SystemDate = String
type DateReg = String
type InfoPred = Integer

-- Parse Date Data
--current_date :: Day
--current_date = parseDay "2022-10-12"

-- List Function for News-Information Main Database Updation
list_newsmaindb :: [NewsMainDatabase]
list_newsmaindb = []
list_infolog :: [InfoLogs]
list_infolog = []
list_newsmaindb_registered :: [ObservationRegistered]
list_newsmaindb_registered = []


-- Check and Count how many values in two lists are equal
checkIfContains :: [String] -> String -> Integer
checkIfContains x y = case elemIndex y x of
                    Nothing -> 0
                    Just n  -> 1

checkIfContainsList :: [String] -> [String] -> Integer
checkIfContainsList _ [] = 0
checkIfContainsList [] _ = 0
checkIfContainsList x (y:ys) = if (checkIfContains x y >= 1) then 1 + checkIfContainsList x ys else checkIfContainsList x ys


-- Makes comparisons News-Information Logs and News-Information Main Database to discover "Fake" or Untrusted Information as part of recommender 
-- (in the search by name -> Author-Publisher  | returns whether the Author-Publisher with Trusted Verified & Validation )

compareHeadLinesAll :: InfoLogs -> [NewsMainDatabase] -> [NewsRecommender]
compareHeadLinesAll _ [] = []
compareHeadLinesAll (xn,xd,xp) ((yn,yc,yd,yp):ys) = ((xn::AuthorName,yc::InfoType,checkIfContainsList xd yd::InfoPred)::NewsRecommender):compareHeadLinesAll (xn,xd,xp) ys

compareHeadLinesAllByName :: String -> [InfoLogs] -> [NewsMainDatabase] -> [NewsRecommender]
compareHeadLinesAllByName name [] [] = []
compareHeadLinesAllByName name [] _ = []
compareHeadLinesAllByName name ((xn,xd,xp):xs) y = if (xn == name) then (compareHeadLinesAll (xn,xd,xp) y) ++ (compareHeadLinesAllByName name xs y) else (compareHeadLinesAllByName name xs y) 

max_list :: [NewsRecommender] -> String
max_list [(n,v,c)] = "Author-Publisher: " ++ n ++ " -- Information Context Category: " ++ v 
max_list ((x1,v1,c1):(x2,v2,c2):xs) = if (c1 > c2) then max_list ((x1,v1,c1):xs) else max_list((x2,v2,c2):xs)


-- Makes comparisons News-Information Logs and News-Information Main Database to discover "Fake" or Untrusted Information  as part of recommender  
-- of Author-Publisher  (returns a list of Author-Publisher and News-Information HeadLines )

newsCatCompareHeadLinesAll :: InfoLogs -> [NewsMainDatabase] -> [ObservationRegisteredByName]
newsCatCompareHeadLinesAll _ [] = []
newsCatCompareHeadLinesAll (xn,xd,xp) ((yn,yc,yd,yp):ys) = ((xn::AuthorName,xd::HeadLines,checkIfContainsList xd yd::InfoPred, yp::NewsCat, yc::InfoType)::ObservationRegisteredByName):newsCatCompareHeadLinesAll (xn,xd,xp) ys

newsCatcompareHeadLinesAllByName :: String -> [InfoLogs] -> [NewsMainDatabase] -> [ObservationRegisteredByName]
newsCatcompareHeadLinesAllByName name [] [] = []
newsCatcompareHeadLinesAllByName name [] _ = []
newsCatcompareHeadLinesAllByName name _ [] = []
newsCatcompareHeadLinesAllByName name ((xn,xd,xp):xs) y = if (xn == name) then (newsCatCompareHeadLinesAll (xn,xd,xp) y) ++ (newsCatcompareHeadLinesAllByName name xs y) else (newsCatcompareHeadLinesAllByName name xs y) 

newsCatMax_list :: [ObservationRegisteredByName] -> String
newsCatMax_list [(n,s,c,q,v)] = q
newsCatMax_list ((n1,s1,c1,q1,v1):(n2,s2,c2,q2,v2):xs) = if (c1 > c2) then newsCatMax_list ((n1,s1,c1,q1,v1):xs) else newsCatMax_list((n2,s2,c2,q2,v2):xs)

infoHeadLinesMax_listGraph :: [ObservationRegisteredByName] -> String
infoHeadLinesMax_listGraph [(n,s,c,q,v)] = "\n Author-Publisher:  " ++ n  ++ "\n -- Information Context Category: " ++ v ++ "\n -- Trustee Information Category: " ++ q ++ ";" ++ "\n "
infoHeadLinesMax_listGraph ((n1,s1,c1,q1,v1):(n2,s2,c2,q2,v2):xs) = if (c1 > c2) then infoHeadLinesMax_listGraph ((n1,s1,c1,q1,v1):xs) else infoHeadLinesMax_listGraph((n2,s2,c2,q2,v2):xs)


-- Function That converts DATA-STRING to DATA-DAY
parseDay :: String -> Day
parseDay s = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" s


-- Functions to Update News-Information Main Database
insert_newsmaindb :: IO()
insert_newsmaindb = do clearScreen
                       now <- getCurrentTime
                       let (year, month, day) = toGregorian $ utctDay now
                       putStr "***************************************************** \n" 
                       putStr "** Global News Category Main Database Registration ** \n"
                       putStr ("**         Current System Date: " ++ show day ++ "-" ++ show month ++ "-" ++ show year ++ "         **\n")
                       putStr "***************************************************** \n" 
                       putStr "News Author/Published : (xxxx-xxxx-xxxx) \n"
                       n <- getLine
                       putStr "Information Context Category: (0-Positive, 2-Neutral, 3-Negative) \n"
                       c <- getLine
                       putStr "News-Information HeadLines :   \n"
                       d <- getLine
                       putStr "News Trustee Category: (0-Trusteed, 2-Fake News) \n"
                       p <- getLine
                       appendFile "GlobalNewsMainDB.txt" (n ++ "\t" ++ c ++ "\t" ++ d ++ "\t" ++ p ++ "\n")
                       putStr "Add another News/ Information? (y/n): \n"
                       resp <- getLine
                       if (resp=="y" || resp=="Y") then insert_newsmaindb else return()


-- Functions to Registered Target News-Information in Log files
insert_infolog :: IO()
insert_infolog = do clearScreen
                    now <- getCurrentTime
                    let (year, month, day) = toGregorian $ utctDay now 
                    putStr "************************************************************** \n" 
                    putStr "**  News/ Information to be Verified Log-Report ** \n"
                    putStr ("**             Current System Date: " ++ show day ++ "-" ++ show month ++ "-" ++ show year ++ "              **\n")
                    putStr "************************************************************** \n" 
                    putStr "News Author/Published : (xxxx-xxxx-xxxx) \n"
                    n <- getLine
                    putStr "News/ Information Contents : \n"
                    d <- getLine
                    putStr "News/ Information Log Report Date (yyyy-mm-dd): \n"
                    p <- getLine
                    print("News/ Information Log-Report Processed... \n")
                    appendFile "informationlog.txt" (n ++ "\t" ++ d ++ "\t" ++ p ++ "\n")
                    print("News/ Information Log-Report Saved Success! \n")
                    putStr "Adding Other News/ Information Log-Report ? : \n"
                    resp <- getLine
                    if (resp=="y" || resp=="Y") then insert_infolog else return()


-- Functions to load News-Information Logs (from TXT files )
loadTab_infolog = do s <-readFile "informationlog.txt"
                     return (gerlist_infolog (map words (lines s)))

gerlist_infolog [] = []
gerlist_infolog ([n,d,p]:xs) = ((n::AuthorName,split d::HeadLines,p::SystemDate)::InfoLogs):(gerlist_infolog xs)

gerlist_newscat [] = []
gerlist_newscat ([n,d,p]:xs) = ((n::AuthorName,split d::HeadLines,p::SystemDate)::InfoLogs):(gerlist_infolog xs)

-- Functions to load News-Information Main Database (from TXT files )
loadTab_newsmaindb = do s <-readFile "GlobalNewsMainDB.txt"
                        return (gerlist_newsmaindatabase (map words (lines s)))

gerlist_newsmaindatabase [] = []
gerlist_newsmaindatabase ([n,c,d,p]:xs) = ((n::AuthorName,c::InfoType,split d::HeadLines,p::NewsCat)::NewsMainDatabase):(gerlist_newsmaindatabase xs)


-- Function to load News-Information Recommendation -> Based on News-Information Main Database.
loadTab_newstype p d = do return (isNewsCat p d)

isNewsCat :: [InfoLogs] -> [NewsMainDatabase] -> [ObservationRegistered]
isNewsCat [] [] = []
isNewsCat _ [] = []
isNewsCat [] _ = []
isNewsCat ((xn,xd,xp):xs) y = if (newsCatMax_list (newsCatcompareHeadLinesAllByName xn ((xn,xd,xp):xs) y) == "1-Fake") then return ((xn::AuthorName,xp::SystemDate,showGregorian (addDays 40 (parseDay xp))::DateReg)::ObservationRegistered) ++ isNewsCat xs y else isNewsCat xs y


-- Functions to Print or Display News-Information Main Database
print_lst_newsmaindb [] =""
print_lst_newsmaindb ((n,c,d,p):xs) = "\nAuthor-Publisher: " ++ n ++ "\n Information Type = " ++ c ++ "\n HeadLines: " ++ print_observation_each d ++ "\n Category Recommendation: " ++ p ++ "\n" ++ (print_lst_newsmaindb xs)

print_lst_newstype_newDate_still date [] =""
print_lst_newstype_newDate_still date ((n,c,e):xs) = if ( (diffDays date (parseDay e)) <= 0) then "Author-Publisher: " ++ n ++ ", Date Registered = " ++ c ++ "\n" ++ (print_lst_newstype_newDate_still date xs) else (print_lst_newstype_newDate_still date xs)

print_lst_newstype_newDate_out date [] =""
print_lst_newstype_newDate_out date ((n,c,e):xs) = if ( (diffDays date (parseDay e)) >= 0) then "Author-Pubisher: " ++ n ++ ", Date Registered = " ++ c ++ "\n" ++ (print_lst_newstype_newDate_out date xs) else (print_lst_newstype_newDate_out date xs)


-- Function to counts News-Information which categorized as Untrusted Information ("Fake")
count_newsCat [] = 0
count_newsCat (x:xs) = 1 + count_newsCat xs


-- Function that converts a STRING to [STRING] (entering Observation Characteristic separated by commas in a LIST)
split str = case break (==',') str of
                (a, ',':b) -> a : split b
                (a, "")    -> [a]

print_observation_each [] = ""
print_observation_each (x:xs) = x ++ ", " ++ print_observation_each xs


-- Function that loads the TXT with the current system date
load_date = do s <-readFile "datesystem.txt"
               return (parseDay s)


-- Function that generates a graph of ( Author-Publisher Name -> Information Type -> Trusted Category Recommendation )
graph :: [InfoLogs] -> [NewsMainDatabase] -> String
graph [] [] = ""
graph _ [] = ""
graph [] _ = ""
graph ((xn,xd,xp):xs) y = infoHeadLinesMax_listGraph (newsCatcompareHeadLinesAllByName xn ((xn,xd,xp):xs) y) ++ graph xs y


-- Main function that contains all the actions of the program
main :: IO()
main = do list_infolog <- loadTab_infolog
          list_newsmaindb <- loadTab_newsmaindb
          now <- getCurrentTime
          let (year, month, day) = toGregorian $ utctDay now
          list_newsmaindb_registered <- loadTab_newstype list_infolog list_newsmaindb         
          putStrLn(" ")
          putStrLn "********************************************************"     
          putStrLn "**  TRUSTEE NEWS-INFORMATION RECOMMENDER APPLICATION  **"
          putStrLn "**    TRUSTED & TRANSPARENT NEWS DIGITAL MEDIA DAO    **"         
          putStrLn "**           Developed By: Raditya Pratama            **"
          putStrLn "**    Electronic & Instrumentation - MIPA UGM 2020    **"
          putStr ("**                Date: " ++ show day ++ "-" ++ show month ++ "-" ++ show year ++ "                    **\n")
          putStrLn "********************************************************"
          putStr "[1] Update News-Information to News Main Database \n"
          putStr "[2] Add News/ Information to be Verified \n"
          putStr "[3] Display Library Collection Main Database \n"
          putStr "[4] Counting Total Instrument/ Equipment Required Recalibration \n"
          putStr "[5] Search News/ Information  \n"
          putStr "[6] Updated Date System \n"
          putStr "[7] Infographic News/ Information Recommendation Insight\n"
          putStr "Option Selection: \n"
          resp <- getLine
          if (resp=="1") then do insert_newsmaindb
                              
          else if (resp=="2") then do insert_infolog

          else if (resp=="3") then do putStr(print_lst_newsmaindb list_newsmaindb)

          else if (resp=="4") then do putStr("Total News which Identified as Un-trusted Information: ")
                                      print(count_newsCat list_newsmaindb_registered)

          else if (resp=="5") then do putStr "Search Author-Publisher: \n"
                                      name <- getLine
                                      let results = compareHeadLinesAllByName name list_infolog list_newsmaindb
                                      if (results == []) then print("Author-Publisher Not Found") else print(max_list results)

          else if (resp=="6") then do putStr "Set Date (yyyy-mm-dd): \n"
                                      newdata <- getLine
                                      let newdate = parseDay newdata
                                      putStr "\nUpdated System DateTime: \n"
                                      writeFile "datesystem.txt" (showGregorian newdate)
                                      let newlist = print_lst_newstype_newDate_still newdate list_newsmaindb_registered
                                      if (newlist == "") then putStr(" - Author-Publisher is not Recorded.\n") else putStr(newlist)
                                      
                                      putStr "\nUpdated Database with Current New Date]:\n"
                                      let newlist = print_lst_newstype_newDate_out newdate list_newsmaindb_registered
                                      if (newlist == "") then putStr(" - Author-Publisher is not Recorded.\n") else putStr(newlist)

          else if (resp=="7") then do putStrLn("\n ** News/ Information Recommendation Insight Infographic ** \n" ++ graph list_infolog list_newsmaindb ++"\n ************** Information-Recommender-System **************\n")                                 

          else error "Selected Option Not Found \n"

          putStr "\nDo you wish to continue (y/n)? : \n"
          resp <- getLine
          if (resp=="y" || resp=="Y") then main else return()