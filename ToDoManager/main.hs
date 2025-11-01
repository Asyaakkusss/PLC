-- Kaia Kanj (kmk233) and Asya Akkus (aya29)

import System.IO

-- Recursively print each task
printTasks :: [String] -> IO ()
printTasks [] = return ()
printTasks (x:xs) = do
  putStrLn (" " ++ x)
  printTasks xs

-- Recursively search for a task
searchTasks :: String -> [String] -> Bool
searchTasks _ [] = False
searchTasks q (x:xs) =
  if q == x
    then True
    else searchTasks q xs  

main :: IO ()
main = do
  putStrLn "Welcome to Task Manager 3000"
  menu []

menu :: [String] -> IO ()
menu tasks = do
  putStrLn "Below are the options:"
  putStrLn " add"
  putStrLn " print"
  putStrLn " search"
  putStr "Enter option:\n"
  hFlush stdout
  option <- getLine

  if option == "add" || option == "Add" -- added in both upper and lower case incase user inputs either 
    then do
      putStr "Enter Task to Add:\n"
      hFlush stdout
      newTask <- getLine
      let newList = newTask : tasks
      menu newList

    else if option == "print" || option == "Print"
      then do
        putStrLn "Here are your tasks:"
        printTasks tasks
        menu tasks

      else if option == "search" || option == "Search"
        then do
          putStr "Enter Task to Search:\n"
          hFlush stdout
          query <- getLine
          if searchTasks query tasks
            then putStrLn ("Found " ++ query)
            else putStrLn ("Could not find " ++ query)
          menu tasks

        else do
          putStrLn "Error"
          menu tasks
