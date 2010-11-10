module Main where

import System.Directory(copyFile,doesDirectoryExist,createDirectory,getDirectoryContents,getHomeDirectory)
import System.Posix.Files(setFileTimes,FileStatus,getFileStatus,modificationTime,isDirectory,isRegularFile)
import System.Posix.Types(EpochTime)
import System.FilePath((</>),replaceDirectory,takeExtension)
import System.Locale(defaultTimeLocale)
import System.Environment(getArgs)
import Control.Monad(unless,msum)
import Control.Monad.Reader(runReaderT,ReaderT,ask)
import Control.Monad.Trans(liftIO)
import Control.Monad.Maybe(runMaybeT,MaybeT(..))
import Control.Exception(handle,SomeException(..))
import Data.Char(toLower)
import Data.Ord(comparing)
import Data.List(groupBy,sortBy)
-- import Data.Function
-- import Data.Time.Clock
import Data.Time.Clock.POSIX(posixSecondsToUTCTime,utcTimeToPOSIXSeconds)
import Data.Time.LocalTime(getCurrentTimeZone,LocalTime,utcToLocalTime,TimeZone)
import Data.Time.Format(parseTime)
import Graphics.Exif as Exif (fromFile,getTag)

-- The Reader/IO combined monad, where Reader stores my options.
type OPIO = ReaderT Opts IO

data Opts = Opts {
    verbose   :: String -> IO (),
    isDryRun  :: Bool,
    seuil     :: Int,
    tzone     :: TimeZone
  }

putLog :: String -> OPIO ()
putLog s = do
  opts <- ask
  liftIO $ verbose opts s

putLogLn :: String -> OPIO ()
putLogLn s = putLog (s++"\n")

notDryRun :: IO () -> OPIO ()
notDryRun action = do
  opts <- ask
  unless (isDryRun opts) $ liftIO action


-- | Generic traversal of directory and its subdirectories
-- must be supplied a function that provides
-- a list of results, and
-- a list of subdirectories to traverse
-- the results from all subdirectories are concatened

getFiles :: Monad m
         => (FilePath -> m ([a], [FilePath]))  -- ^ Get (files, subdirectories) of some directory
         -> FilePath                           -- ^ Root path
         -> m [ a ]

getFiles lsdir = loop where
  loop path = do
    (files, subdirs) <- lsdir path
    dirs             <- mapM loop subdirs
    return $ concat (files : dirs )

-- | Returns a list of images, with its date and time.
-- If the date and time can be found in the exif data, we use it.
-- Otherwise, we use the file's date and time

getFilesIO :: FilePath -> OPIO [ (FilePath,EpochTime) ]
getFilesIO = getFiles lsdir where

  lsdir dir = do
    putLog $ "directory : " ++ dir ++ " ... "
    contents <- liftIO $ getDirectoryContents dir
    let valid n = notElem n [".", ".."]
        isOk  n = elem (map toLower $ takeExtension n) [".jpg",".jpeg"]   

    stats <- mapM (getStats . (dir </>) ) (filter valid contents)

    let directories = [p | Just (p, stat) <- stats, isDirectory stat]
        files       = [(p,stat) | Just (p, stat) <- stats, isOk p,isRegularFile stat]

    exifs <- mapM getTime files 

    putLogLn $ show ( length files) ++ " files, " ++ show ( length directories) ++ " directories."
    return (exifs, directories)

-- | Associates a filename with its IO status
-- The status can tell us wether the file is a directory or a regular file,
-- its size, modification date and time, etc.
getStats :: FilePath -> OPIO (Maybe (FilePath, FileStatus))
getStats path = liftIO $  handle (\(SomeException _) -> return Nothing) $ do
  status <- getFileStatus path
  return $ Just (path, status)

-- | Gets the date and time of a file and its status.
-- Prefers the date and time from exif data.
getTime :: (FilePath, FileStatus) -> OPIO (FilePath, EpochTime)
getTime (path,stat) = do
  let ftime                 = modificationTime stat
      err (SomeException _) = return ftime
  time <- liftIO $ handle err $ do
    exif <- Exif.fromFile path
    let getExifTime = MaybeT . liftIO . Exif.getTag exif
    res <- runMaybeT $ do
      tmp <- msum . map getExifTime $ [ "DateTimeOriginal", "DateTimeDigitized", "DateTime" ]
      MaybeT . return . parseTime defaultTimeLocale "%Y:%m:%d %H:%M:%S" $ tmp
    case res of
      Nothing    -> return ftime
      Just etime -> return . fromIntegral . fromEnum . utcTimeToPOSIXSeconds $ etime
  return (path,time)

-- | Sorts a list of photos,datetime by datetime.
sortDate :: [(FilePath,EpochTime)] -> [(FilePath,EpochTime)]
sortDate   = sortBy (comparing snd)

-- | Computes the difference in time between two photos
-- returns a triple of the photo, datetime, and delta
deltaDate :: [(FilePath,EpochTime)] -> [(FilePath,EpochTime,Int)]
deltaDate [] = []
deltaDate ((p0,d0):xs) = (p0,d0,0) : loop d0 xs
  where loop a ((p,d):ys) = (p,d,fromEnum $ d-a) : loop d ys
        loop _ []           = []

-- | converts the datetime format so that we can "show" it
-- toLocal :: TimeZone -> [(FilePath,UTCTime,NominalDiffTime)] -> [(FilePath,LocalTime,NominalDiffTime)]
-- toLocal tz = map (\(a,b,c) -> (a,utcToLocalTime tz b,c))


groupPhoto :: Int -> [(FilePath, EpochTime,Int)] -> [[(FilePath,EpochTime)]]
groupPhoto s = (map.map) simplify . groupBy ok
  where
    ok _ (_,_,i) = i < s
    simplify (a,b,_) = (a,b)

copyGroup :: FilePath -> [(FilePath,EpochTime)] -> OPIO ()
copyGroup _ [] = return ()
copyGroup root l = do
  -- Check if the year directory exists, or create it
  -- Check if the day directory exists, or number it
  -- Copy the files
  opts <- ask
  let tz   = tzone opts
      ds   = show . utcToLocalTime tz . posixSecondsToUTCTime .realToFrac . snd . head $ l
      date = take 10 ds
      year = take 4 date
      ydir = root </> year
      ddir = ydir </> date

  putLog $ "Checking directory " ++ ydir ++ "..."
  ok <- liftIO $ doesDirectoryExist ydir
  if ok then putLogLn "ok."
        else do liftIO $ createDirectory ydir
                putLogLn "created."

  ddir' <- checkOrCreateDir ddir

  let copy (file,etime) = do
      let dst = replaceDirectory file ddir'
      putLogLn $ "copying " ++ file ++ " to " ++ dst
      notDryRun $ do
        copyFile file dst
        setFileTimes dst etime etime

  mapM_ copy l

-- | Checks if a directory exists. If it doesnt, creates it.
-- Now if it does, append a number to the directory, and try again

checkOrCreateDir :: FilePath -> OPIO FilePath
checkOrCreateDir path = go (1::Int)
  where
    go n = do
      let path' = path ++ if n > 1 then ' ' : show n else ""
      putLog $ "Checking directory " ++ path' ++ "..."
      ok <- liftIO $ doesDirectoryExist path'
      if ok then do putLogLn "exists."
                    go (n+1)
            else do liftIO $ createDirectory path'
                    putLogLn "created."
                    return path'

-- | Does all the stuff
run :: Opts -> FilePath -> FilePath -> IO ()
run opts srcPath dstPath = flip runReaderT opts $ do
    l <- getFilesIO srcPath
    let s = seuil opts
        l'  = groupPhoto s . deltaDate . sortDate $ l
    mapM_ (copyGroup dstPath) l'


aff :: (LocalTime, [FilePath]) -> IO ()
aff (a,b) = putStrLn $ show a ++ "  " ++ show ( length b) ++ " photo(s)."


mkOpts :: (String -> IO ())
       -> Bool
       -> Int
       -> IO Opts

mkOpts f d s = do
  tz <- getCurrentTimeZone
  return Opts { verbose=f , isDryRun = d , seuil=s, tzone=tz }

-- test3 :: IO [(String, String)]
-- test3 = Exif.fromFile "/data/perso/media/imatrier/img_1525.jpg" >>= Exif.allTags

test4 :: IO ()
test4 = do
  opts <- mkOpts putStr False 7000
  run opts "/data/media/photos_a_trier/dcimZzzZZ"
           "/data/media/testphotos"

main :: IO ()
main = do
  home <- getHomeDirectory
  let cfg_file = home </> ".photocop"
  cfg <- handle (\(SomeException _) -> return "") $ readFile cfg_file
  case lines cfg of
    [ d,s ] -> args d (read s :: Int)
    _       -> putStrLn $ "config file (" ++ cfg_file ++ ") missing."

args :: FilePath -> Int -> IO ()
args d s = do
  a <- getArgs
  case a of
    [ src ] -> do o <- mkOpts putStr False s
                  run o src d 
    _       -> putStrLn "Usage : photocop source_directory."

  