import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import Data.List (isInfixOf)

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
  putStrLn "\nSearch Something Here"
  query <- getLine
  let results = filter (isInfixOf query) index
  putStrLn "\nSearch Result :"
  mapM_ putStrLn results