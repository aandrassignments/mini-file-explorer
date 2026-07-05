import System.Directory
import System.FilePath
import Data.Char
import Data.List

----------------------------------------------------
-- Scan seluruh folder secara rekursif
----------------------------------------------------

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

----------------------------------------------------
-- Membaca isi file
----------------------------------------------------

readDocument :: FilePath -> IO (FilePath, String)
readDocument file = do
    content <- readFile file
    return (file, map toLower content)

----------------------------------------------------
-- Membuat index
----------------------------------------------------

buildIndex :: [FilePath] -> IO [(FilePath,String)]
buildIndex files =
    mapM readDocument files

----------------------------------------------------
-- Search berdasarkan nama file atau isi file
----------------------------------------------------

searchDocs :: String -> [(FilePath,String)] -> [FilePath]
searchDocs keyword docs =
    map fst $
    filter cocok docs
  where
    key = map toLower keyword

    cocok (path,content) =
        key `isInfixOf` map toLower (takeFileName path)
        ||
        key `isInfixOf` content

----------------------------------------------------
-- Statistik
----------------------------------------------------

countWords :: [(FilePath,String)] -> Int
countWords docs =
    foldl (+) 0 $
    map hitung docs
  where
    hitung (_,txt) =
        length (words txt)

----------------------------------------------------
-- Menampilkan seluruh dokumen
----------------------------------------------------

showDocuments :: [(FilePath,String)] -> IO ()

showDocuments docs = do
    putStrLn ""

    mapM_ tampil docs

    where
        tampil (path,_) =
            putStrLn path

----------------------------------------------------
-- Menu
----------------------------------------------------

menu :: [(FilePath,String)] -> IO ()

menu docs = do

    putStrLn ""
    putStrLn "==============================="
    putStrLn "      MINI SEARCH ENGINE"
    putStrLn "==============================="
    putStrLn "1. Cari Dokumen"
    putStrLn "2. Lihat Semua Dokumen"
    putStrLn "3. Jumlah Dokumen"
    putStrLn "4. Total Kata"
    putStrLn "5. Keluar"

    putStr "Pilihan : "

    choice <- getLine

    case choice of

        "1" -> do

            putStr "Masukkan keyword : "
            key <- getLine

            let result = searchDocs key docs

            if null result
                then putStrLn "\nDokumen tidak ditemukan."
                else do
                    putStrLn "\nHasil pencarian:"
                    mapM_ putStrLn result

            menu docs

        "2" -> do

            putStrLn "\nDaftar Dokumen:"
            showDocuments docs

            menu docs

        "3" -> do

            putStrLn $
                "\nJumlah dokumen : "
                ++ show (length docs)

            menu docs

        "4" -> do

            putStrLn $
                "\nTotal kata : "
                ++ show (countWords docs)

            menu docs

        "5" -> do
            putStrLn "\nTerima kasih."

        _ -> do

            putStrLn "\nPilihan tidak valid."

            menu docs

----------------------------------------------------
-- Main
----------------------------------------------------

main :: IO ()

main = do

    putStrLn "Scanning folder documents..."

    files <- scanFolder "documents"

    putStrLn $
        "Ditemukan "
        ++ show (length files)
        ++ " file."

    index <- buildIndex files

    menu index