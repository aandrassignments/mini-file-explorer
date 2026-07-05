import System.Directory
import System.FilePath
import Data.Char
import Data.List

-----------------------------------------
-- Mengambil seluruh file dalam folder
-----------------------------------------

scanFolder :: FilePath -> IO [FilePath]
scanFolder path = do
    items <- listDirectory path
    processItems path items

processItems :: FilePath -> [FilePath] -> IO [FilePath]
processItems _ [] = return []

processItems path (x:xs) = do
    let full = path </> x
    isDir <- doesDirectoryExist full

    rest <- processItems path xs

    if isDir
        then do
            inside <- scanFolder full
            return (inside ++ rest)
        else
            return (full : rest)

-----------------------------------------
-- Membaca isi file
-----------------------------------------

readDocument :: FilePath -> IO (FilePath,String)
readDocument file = do
    content <- readFile file
    return (file,map toLower content)

-----------------------------------------
-- Membuat index
-----------------------------------------

buildIndex :: [FilePath] -> IO [(FilePath,String)]
buildIndex files = mapM readDocument files

-----------------------------------------
-- Search
-----------------------------------------

searchDocs :: String -> [(FilePath,String)] -> [FilePath]
searchDocs keyword docs =
    map fst $
    filter contains docs
    where
        key = map toLower keyword

        contains (_,content) =
            isInfixOf key content

-----------------------------------------
-- Statistik
-----------------------------------------

countWords :: [(FilePath,String)] -> Int
countWords docs =
    foldl (+) 0 $
    map wordCount docs
    where
        wordCount (_,txt) =
            length (words txt)

-----------------------------------------
-- Menu
-----------------------------------------

menu :: [(FilePath,String)] -> IO ()

menu docs = do

    putStrLn "\n===== MINI SEARCH ENGINE ====="
    putStrLn "1. Cari Kata"
    putStrLn "2. Jumlah Dokumen"
    putStrLn "3. Total Kata"
    putStrLn "4. Keluar"

    putStr "Pilihan : "

    choice <- getLine

    case choice of

        "1" -> do
            putStr "Masukkan keyword : "
            key <- getLine

            let result = searchDocs key docs

            if null result
                then putStrLn "\nTidak ditemukan."
                else do
                    putStrLn "\nHasil:"
                    mapM_ putStrLn result

            menu docs

        "2" -> do
            putStrLn $
                "\nJumlah dokumen : "
                ++ show (length docs)

            menu docs

        "3" -> do
            putStrLn $
                "\nTotal kata : "
                ++ show (countWords docs)

            menu docs

        "4" -> do
            putStrLn "Terima kasih."

        _ -> do
            putStrLn "Pilihan salah."
            menu docs

-----------------------------------------
-- Main
-----------------------------------------

main :: IO ()

main = do

    files <- scanFolder "documents"

    index <- buildIndex files

    menu index