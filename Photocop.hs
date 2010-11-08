module Main where

import System.Directory
import System.Posix.Files
import System.Posix.Time
import System.Posix.Types
import System.FilePath
import System.Locale
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Maybe
import Control.Exception
import Data.Char
import Data.List
import Data.Function
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import Data.Time.Format 
import Graphics.Exif as Exif

getFiles :: Monad m
         => (FilePath -> m ([a], [FilePath]))  -- ^ Get (files, subdirectories) of some directory
         -> FilePath                           -- ^ Root path
         -> m [ a ]
getFiles lsdir = loop where
  loop path = do
    (files, subdirs) <- lsdir path
    dirs             <- mapM loop subdirs
    return $ concat (files : dirs )


getFilesIO :: TimeZone -> FilePath -> IO [ (FilePath,UTCTime) ]
getFilesIO timezone = getFiles lsdir where

  lsdir dir = do
    putStr $ "directory : " ++ dir ++ " ... "
    contents <- getDirectoryContents dir
    let valid n = not (elem n [".", ".."])
        isOk  n = elem (map toLower $ takeExtension n) [".jpg",".jpeg"]   

    stats <- mapM (getStats . (dir </>) ) (filter valid contents)

    let directories = [p | Just (p, stat) <- stats, isDirectory stat]
        files       = [(p,stat) | Just (p, stat) <- stats, isOk p,isRegularFile stat]

    exifs <- mapM getTime files 

    putStrLn $ (show $ length files) ++ " files, " ++ (show $ length directories) ++ " directories."
    return (exifs, directories)

getStats path = do
  handle (\(SomeException _) -> return Nothing) $ do
    status <- getFileStatus path
    return $ Just (path, status)

getTime (path,stat) = do
  let ftime = posixSecondsToUTCTime .realToFrac . modificationTime $ stat
  time <- handle (\(SomeException _) -> return ftime) $ do
    exif <- Exif.fromFile path
    let getExifTime = MaybeT . liftIO . Exif.getTag exif
    res <- runMaybeT $ do
      tmp <- msum . map getExifTime $ [ "DateTimeOriginal", "DateTimeDigitized", "DateTime" ]
      MaybeT . return . parseTime defaultTimeLocale "%Y:%m:%d %H:%M:%S" $ tmp
    case res of
      Nothing    -> return ftime
      Just etime -> return etime
  return (path,time)

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

test p = do
  tz <- getCurrentTimeZone
  l <- getFilesIO tz p
  let l'  = toLocal tz . deltaDate . triDate $ l
  let l'' = groupPhoto 7000 l'
  return l''
  
test1 = test "/data/media/photos/1999"
test2 = test "/data/img"

test3 = Exif.fromFile "/data/perso/media/imatrier/img_1525.jpg" >>= Exif.allTags
  
