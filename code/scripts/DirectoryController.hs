module DirectoryController (finder, getDirectories) where

import Data.List
import System.IO
import System.Directory

type FilterPrefix = String
type EntryData = String
type FileName = FilePath
type FolderName = FilePath

type ClassInstance = String

type FileInstance = String

-- iterates through each drasil- package and outputs subdirectories and haskell files
-- remove this function later
iterator :: [FilePath] -> IO [a0]
iterator (x:xs) = do 
  setCurrentDirectory "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code"
  some <- listDirectory x
  wd <- getCurrentDirectory
  
  let l = filter (isInfixOf ".") some
      d = map ((++ x) . (++"/") . (++ wd) . (++" ")) (some \\ l)
      h = filter (isSuffixOf ".hs") some
  
  print x -- << drasil- package
  print d -- << sub-folders
  print h -- << haskell files

  if xs /= [] then iterator xs
  else return []

-- iterates through each drasil- package and outputs subdirectories and haskell files
iterator2 :: FilePath -> IO ([FolderName],[FileName])
iterator2 nameFilePath = do
  let [fn,wd] = words nameFilePath
  setCurrentDirectory wd
  fc <- listDirectory fn

  let fcs = (a,b)
      a = map ((++fn) . (++"/") . (++wd) . (++" ")) $ fc \\ filter (isInfixOf ".") fc
      b = map ((++fn) . (++"/") . (++wd) . (++" ")) $ filter (isSuffixOf ".hs") fc
  return fcs

finder :: FolderName -> IO [FileName]
finder folderName = do
  rawData <- iterator2 folderName

  let rawFolders = fst rawData
  folders <- verifyDirectories rawFolders
  rawFiles <- mapM finder folders

  let bakedFiles
        | null rawFiles = []
        | otherwise = concat rawFiles

  let files
        | not (null folders) = snd rawData ++ bakedFiles
        | otherwise = snd rawData
  return files

-- gets all drasil- packages + filepaths in a list
getDirectories :: FilePath -> FilterPrefix -> IO [FilePath]
getDirectories directoryPath filterPrefix = do
  all <- listDirectory directoryPath
  -- filters all drasil- packages
  let filtered = map ((++ directoryPath) . (++" ")) $ filter (isPrefixOf filterPrefix) all
  return filtered

-- verifies that each folder/directory exists
verifyDirectories :: [FilePath] -> IO [FilePath]
verifyDirectories rawFolders = do
  let rawDirectories = map (joins . words) rawFolders
  boolFolders <- mapM doesDirectoryExist rawDirectories
  let verifiedDirectories = snd $ partition (null) (zipWith fBool boolFolders rawFolders)
  return verifiedDirectories

-- combines lists of Booleans and FilePaths (if True, FilePath exists)
fBool :: Bool -> FilePath -> FilePath
fBool b s = if b then s else ""

-- combines folder name with filepath (for testing if directory exists)
joins :: [FilePath] -> FilePath
joins (a:b:_) = b ++ "/" ++ a
