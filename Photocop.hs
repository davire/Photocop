module Main where

import System.Directory
import System.Posix.Files
import System.Posix.Time
import System.Posix.Types
import System.FilePath
import Control.Monad
import Control.Exception
import Data.Char
import Data.List
import Data.Function
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

getFiles :: Monad m
         => (FilePath -> m ([a], [FilePath]))  -- ^ Get (files, subdirectories) of some directory
         -> FilePath                           -- ^ Root path
         -> m [ a ]
getFiles lsdir = loop where
  loop path = do
    (files, subdirs) <- lsdir path
    dirs             <- mapM loop subdirs
    return $ concat (files : dirs )


--getFilesIO :: FilePath -> IO [ (FilePath,EpochTime) ]
getFilesIO = getFiles lsdir where

  lsdir dir = do
    putStr $ "directory : " ++ dir ++ " ... "
    contents <- getDirectoryContents dir
    let valid n = not (elem n [".", ".."])
        isOk  n = elem (map toLower $ takeExtension n) [".jpg",".jpeg"]   

    stats <- forM (filter valid contents) $ \name -> do
      let path = dir </> name
      handle (\(SomeException _) -> return Nothing) $ do
        status <- getFileStatus path
        return $ Just (path, status)

    let directories = [p | Just (p, stat) <- stats, isDirectory stat]
    let files       = [(p,time stat) | Just (p, stat) <- stats, isOk p,isRegularFile stat]
    putStrLn $ (show $ length files) ++ " files, " ++ (show $ length directories) ++ " directories."
    return (files, directories)

  time = posixSecondsToUTCTime .realToFrac . modificationTime



triDate   = sortBy (compare `on` snd)

deltaDate [] = []
deltaDate ((p,d):xs) = (p,d,0) : loop d xs
  where loop a (x@(p,d):xs) = (p,d,diffUTCTime d a) : loop d xs
        loop _ []           = []

toLocal tz = map (\(a,b,c) -> (a,utcToLocalTime tz b,c))

groupPhoto seuil = newGroup
  where
    newGroup []      = []
    newGroup ((p,d,t):xs) = (d,p:ps) : newGroup xs'
      where
        (r,xs') = span (\(a,b,c) -> c <= seuil) xs
        ps      = map (\(a,_,_) -> a) r

aff (a,b) = putStrLn $ (show a) ++ "  "++(show $ length b)++" photo(s)."

test = do
  l <- getFilesIO "/data/media/photos/1999"
  tz <- getCurrentTimeZone
  let l'  = toLocal tz . deltaDate . triDate $ l
  let l'' = groupPhoto 7000 l'
  return l''
  
  