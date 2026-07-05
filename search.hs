import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath (takeFileName, (</>))
import Data.List (isInfixOf)
import Data.Char (toLower)
import System.IO (hFlush, stdout)

scan :: FilePath -> IO[FilePath]
scan path = do
  items <- listDirectory path
  results <- mapM (process path) items --traverse works too
  return (concat results)

process :: FilePath -> FilePath -> IO[FilePath] 
process path item = do
  let fullPath = path </> item
  isDir <- doesDirectoryExist fullPath
  if isDir
    then do
      subPath <- scan fullPath
      return (fullPath : subPath)
    else return [fullPath]

main = do
  index <- scan "."
  putStrLn "\n|     Mini Search Engine     |"
  putStr "\nEnter Filename : "
  hFlush stdout
  query <- getLine
  putStrLn "\n===== Search Result ====="
  let queryLower = map toLower query
  let results = filter (\p -> isInfixOf queryLower (map toLower p)) index
  if null results
    then putStrLn "no matching files found"
    else do
      putStrLn ("Found " ++ show(length results) ++ " result(s) : ")
      mapM_ (\p -> do
        isDir <- doesDirectoryExist p 
        putStrLn ("File : " ++ takeFileName p)
        putStrLn ("Type : " ++ if isDir then "Folder" else "File") 
        putStrLn("Path : " ++ p) 
        putStrLn ""
        ) results
  