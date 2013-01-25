{-# LANGUAGE BangPatterns #-}
module Photocop where

import System.Directory(copyFile,doesDirectoryExist,createDirectory,getDirectoryContents,getHomeDirectory)
import System.Posix.Files(setFileTimes,FileStatus,getFileStatus,modificationTime,isDirectory,isRegularFile)
import System.Posix.Types(EpochTime)
import System.FilePath((</>),replaceDirectory,takeExtension)
import System.Locale(defaultTimeLocale)
import Control.Monad(unless,msum)
import Control.Monad.Trans(liftIO)
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Maybe(runMaybeT,MaybeT(..))
import Control.Exception(handle,SomeException(..))
import Data.Char(toLower)
import Data.Ord(comparing)
import Data.List(groupBy,sortBy)
import Data.Time.Clock
import Data.Time.Clock.POSIX(posixSecondsToUTCTime,utcTimeToPOSIXSeconds)
import Data.Time.LocalTime(getCurrentTimeZone,LocalTime,utcToLocalTime,TimeZone)
import Data.Time.Format(parseTime)
import Graphics.Exif as Exif (fromFile,getTag)


--
-- putLog :: String -> OPIO ()
-- putLog s = putLog (s++"\n")

-- Actions to perform or not depending on the options
notDryRun :: IO () -> IO ()
notDryRun action = action -- return ()
--   opts <- ask
--   unless (isDryRun opts) $ liftIO action


photocop logUI !threshold !srcdir !dstdir = do
  logUI 0 "Building file list"
  tz    <- getCurrentTimeZone

  -- Obtenir la liste des images
  (l,n) <- getFilesIO logUI tz srcdir
  case l of
    [] -> logUI 0 "No files to copy"
    l' -> do
      -- Regrouper les photos entre elles
      let l'  = groupPhoto threshold . deltaDate . sortDate $! l
      logUI 0 $ "Copying "++show n++" files"

      -- Copier chaque groupe
      mapM_ (copyGroup logUI tz dstdir) l'
  return ()

-- test = do
--   tz <- getCurrentTimeZone
--   l <- getFilesIO putStrLn tz "/data/media/photos_a_trier/101_PANA/"
--   return $ groupPhoto 7000 . deltaDate . sortDate $! l
-- 
-- test2 l = do
--   tz <- getCurrentTimeZone
--   mapM_ (copyGroup putStrLn tz "/data/media/photos/") l
-- 
-- test3 = photocop putStrLn 7000 "/data/media/photos_a_trier/101_PANA" "/data/media/photos"

{-

setFileTimes     : UTCTime
exif             : String
modificationTime : UTCTime
UTCTime -> UTCTime    posixSecondsToUTCTime (realToFrac :: POSIXTime)

-}



-- | Generic traversal of directory and its subdirectories
-- must be supplied a function that provides
-- a list of results, and
-- a list of subdirectories to traverse
-- the results from all subdirectories are concatened
-- Returns: a list of filepath, and the number of files found

getFiles :: Monad m
         => (FilePath -> m ([a], [FilePath], Int))  -- ^ Get (files, subdirectories) of some directory
         -> FilePath                                -- ^ Root path
         -> m ([ a ],Int )

getFiles lsdir = loop where
  loop path = do
    (files, subdirs, !nbf ) <- lsdir path
    lsub                   <- mapM loop subdirs
    let dirs = map fst lsub
        nbf' = sum $ map snd lsub
    return $ ( concat (files : dirs ), nbf+nbf' )

-- | Returns a list of images, with its date and time.
-- If the date and time can be found in the exif data, we use it.
-- Otherwise, we use the file's date and time

-- getFilesIO :: TimeZone -> FilePath -> IO [ (FilePath,UTCTime) ]
getFilesIO logUI tz = getFiles lsdir where

  lsdir dir = do
    logUI 0 $ "directory : " ++ dir ++ " ... "
    contents <- getDirectoryContents dir
    let valid n = notElem n [".", ".."]
        isOk  n = elem (map toLower $ takeExtension n) [".jpg",".jpeg"]

    stats <- mapM (getStats . (dir </>) ) (filter valid contents)

    let directories = [p | Just (p, stat) <- stats, isDirectory stat]
        files       = [(p,stat) | Just (p, stat) <- stats, isOk p,isRegularFile stat]
        nbf         = length files
    exifs <- mapM (getTime tz) files

    logUI 0 $ show nbf ++ " files, " ++ show ( length directories) ++ " directories."
    return (exifs, directories, nbf)

-- | Associates a filename with its IO status
-- The status can tell us wether the file is a directory or a regular file,
-- its size, modification date and time, etc.

getStats :: FilePath -> IO (Maybe (FilePath, FileStatus))
getStats path = handle (\(SomeException _) -> return Nothing) $ do
  status <- getFileStatus path
  return $ Just (path, status)

-- | Gets the date and time of a file and its status.
-- Prefers the date and time from exif data.
-- We get the time as a string from Exif data.
-- parseTime converts it to one of Day,UTCTime,TimeZone,TimeOfDay,ZonedTime or LocalTime
-- From the file modification time we get a EpochTime

getTime :: TimeZone -> (FilePath, FileStatus) -> IO (FilePath, UTCTime)
getTime tz (path,stat) = do
  let ftime                 = epoch2utc . modificationTime $ stat
      err (SomeException _) = return ftime
  time <- liftIO $ handle err $ do
    exif <- Exif.fromFile path
    let getExifTime = MaybeT . liftIO . Exif.getTag exif
    res <- runMaybeT $ do
      tmp <- msum . map getExifTime $ [ "DateTimeOriginal", "DateTimeDigitized", "DateTime" ]
      MaybeT $ return (parseTime defaultTimeLocale "%Y:%m:%d %H:%M:%S" tmp :: Maybe UTCTime)
    case res of
      Nothing    -> return ftime
      Just etime -> return etime
  return (path,time)

-- | Sorts a list of photos,datetime by datetime.
sortDate :: [(FilePath,UTCTime)] -> [(FilePath,UTCTime)]
sortDate   = sortBy (comparing snd)

-- | Computes the difference in time between two photos
-- returns a triple of the photo, datetime, and delta
deltaDate :: [(FilePath,UTCTime)] -> [(FilePath,UTCTime,NominalDiffTime)]
deltaDate [] = []
deltaDate ((p0,d0):xs) = (p0,d0,0) : loop d0 xs
  where loop a ((p,d):ys) = (p,d,diffUTCTime d a) : loop d ys
        loop _ []           = []

-- | converts the datetime format so that we can "show" it
-- toLocal :: TimeZone -> [(FilePath,UTCTime,NominalDiffTime)] -> [(FilePath,LocalTime,NominalDiffTime)]
-- toLocal tz = map (\(a,b,c) -> (a,utcToLocalTime tz b,c))

groupPhoto :: NominalDiffTime -> [(FilePath, UTCTime,NominalDiffTime)] -> [[(FilePath,UTCTime)]]
groupPhoto threshold = (map.map) simplify . groupBy ok
  where
    ok _ (_,_,interval ) = interval < threshold
    simplify (a,b,_) = (a,b)

-- data CF = CFCOPY { cfSource  :: FilePath
--                  , cfRelDest :: FilePath
--                  , cfTime    :: UTCTime }
--         | CFCHECK { cfYear   :: String, cfDate :: String }
-- 
-- mkCopyGroup :: [(FilePath,UTCTime)] -> [ CF ]
-- mkCopyGroup [] = []
-- mkCopyGroup lst@(l:ls) = CFCHECK ydir date : map cp lst
--   where
--       ds   = show $ snd  l
--       date = take 10 ds
--       ydir = take 4 date
--       ddir = ydir </> date
--       cp (f,t) = CFCOPY f ddir t

-- copyFiles = undefined
-- copyFiles [] = return ()
-- copyFiles CFCHECK y d = do
--   logUI 0 $ "Checking year directory " ++ ydir ++ "..."
--   ok <- doesDirectoryExist ydir
--   when not ok $ notDryRun $ createDirectory ydir
--   -- Checks if the day directory exists, or give it a number
--   ddir' <- checkOrCreateDir d
-- 
--   let copy (file,utime) = do
--       let dst = replaceDirectory file ddir'
--       logUI 0 $ "copying " ++ file ++ " to " ++ dst
--       notDryRun $ do
--         copyFile file dst
--         let etime = utc2epoch utime
--         setFileTimes dst etime etime
--   undefined

-- copyGroup :: TimeZone
--           -> FilePath
--           -> [(FilePath,UTCTime)]
--           -> IO ()
copyGroup _ _ _ [] = return ()
copyGroup logUI tz root l = do
  -- Check if the year directory exists, or create it
  -- Check if the day directory exists, or number it
  -- Copy the files
  let ds   = show . snd . head $ l
      date = take 10 ds
      year = take 4 date
      ydir = root </> year
      ddir = ydir </> date

  -- Checks if the year directory exists, or creates it.
  -- Should really check all directory parts.
  logUI 0 $ "First file : " ++ (fst $ head l ) ++ " " ++ ds ++ "..."
  logUI 0 $ "Checking directory " ++ ydir ++ "..."
  ok <- liftIO $ doesDirectoryExist ydir
  if ok then logUI 0 "ok."
        else do liftIO $ notDryRun $ createDirectory ydir
                logUI 0 "created."

  -- Checks if the day directory exists, or give it a number
  ddir' <- checkOrCreateDir logUI ddir

  let copy (file,utime) = do
      let dst = replaceDirectory file ddir'
      logUI 0 $ "copying " ++ file ++ " to " ++ dst
      notDryRun $ do
        copyFile file dst
        let etime = utc2epoch utime
        setFileTimes dst etime etime

 -- Copy the files
  mapM_ copy l

epoch2utc :: EpochTime -> UTCTime
epoch2utc = posixSecondsToUTCTime . realToFrac
-- epoch2Local tz = utcToLocalTime tz . posixSecondsToUTCTime . realToFrac

utc2epoch :: UTCTime -> EpochTime
utc2epoch = fromInteger . round . utcTimeToPOSIXSeconds



-- | Checks if a directory exists. If it doesnt, creates it.
-- Now if it does, append a number to the directory, and try again

-- checkOrCreateDir :: FilePath -> IO FilePath
checkOrCreateDir logUI !path = go (1::Int)
  where
    go n = do
      let path' = path ++ if n > 1 then ' ' : show n else ""
      logUI 0 $ "Checking directory " ++ path' ++ "..."
      ok <- liftIO $ doesDirectoryExist path'
      if ok then do logUI 0 "exists."
                    go (n+1)
            else do liftIO $ notDryRun $ createDirectory path'
                    logUI 0 "created."
                    return path'



aff :: (LocalTime, [FilePath]) -> IO ()
aff (a,b) = putStrLn $ show a ++ "  " ++ show ( length b) ++ " photo(s)."

