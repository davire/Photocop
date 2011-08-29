module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore

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
import Data.Time.Clock
import Data.Time.Clock.POSIX(posixSecondsToUTCTime,utcTimeToPOSIXSeconds)
import Data.Time.LocalTime(getCurrentTimeZone,LocalTime,utcToLocalTime,TimeZone)
import Data.Time.Format(parseTime)
import Graphics.Exif as Exif (fromFile,getTag)

-- The Reader/IO combined monad, where Reader stores my options.
-- type OPIO = ReaderT Opts IO
-- 
-- data Opts = Opts {
--     verbose   :: String -> IO (),
--     isDryRun  :: Bool,
--     threshold  :: Int,
--     tzone     :: TimeZone
--   }

-- Logging functions
putLog :: String -> IO ()
putLog s = do
  logMessage s
  putStrLn s

 -- 
-- putLog :: String -> OPIO ()
-- putLog s = putLog (s++"\n")

-- Actions to perform or not depending on the options
notDryRun :: IO () -> IO ()
notDryRun action = action
--   opts <- ask
--   unless (isDryRun opts) $ liftIO action


main :: IO ()
main = do
--   home <- getHomeDirectory
--   let cfg_file = home </> ".photocop"
--   cfg <- handle (\(SomeException _) -> return "") $ readFile cfg_file
--   case lines cfg of
--     [ d,s ] -> args d (read s :: Int)
--     _       -> putStrLn $ "config file (" ++ cfg_file ++ ") missing."
  a <- getArgs
  case a of
    [ t,src,dst ] -> start $ gui (read t::Int) src dst
    [ src,dst ]   -> start $ gui 7000 src dst
    [ src ]       -> start $ gui 7000 src "/data/media/photos"
    []            -> start $ gui 7000 "/media" "/data/media/photos"

-- mkOpts :: (String -> IO ())
--        -> Bool
--        -> Int
--        -> IO Opts

-- mkOpts f d s = do
--   tz <- getCurrentTimeZone
--   return Opts { verbose=f , isDryRun = d , threshold=s, tzone=tz }

-- args :: FilePath -> Int -> IO ()
-- args d s = do
--     [ src ] -> do o <- mkOpts putStr False s
--                   myrun o src d
--     _       -> putStrLn "Usage : photocop source_directory."

gui thresh_int srcdir dstdir = do
  let threshold = toEnum thresh_int :: NominalDiffTime
  dlg      <- frame [ text := "Photocop", clientSize := sz 300 300 ]
  pause    <- button dlg [ text := "Pause" ]
  stop     <- button dlg [ text := "Stop" ]
  progress <- hgauge dlg 100 []
  current  <- staticText dlg [text := ""]
--   current  <- textEntry dlg [ enabled := False, color := (rgb 6 4 1) ]
  list     <- textCtrl dlg [wrap := WrapNone, enabled := False]
  hidelst  <- button dlg [ text := "Hide" ]
  set dlg [ layout := margin 10 $ column 5  [
        hfill $ widget progress
      , fill  $ widget list
      , hfill $ widget current
      , hfill $ row 5 [
          floatCentre $ widget hidelst
        , floatCentre $ widget pause
        , floatCentre $ widget stop
        ]
      ]
    ]
  set hidelst [ on command := set list [ visible :~ not ] ]
  textCtrlMakeLogActiveTarget list
  -- set stop    [ on command := ... ]
  -- set dlg [ on close := ... ]

  --
  let msg = "Building file list"
  set current [ text := msg ]
  putLog msg
  
  tz <- getCurrentTimeZone
  l <- getFilesIO tz srcdir
  case l of
    [] -> set current [ text := "No files to copy" ]
    l' -> do
      let l'  = groupPhoto threshold . deltaDate . sortDate $! l
      putLog "Copying files"
      mapM_ (copyGroup tz dstdir) l'
  return ()

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

getFilesIO :: TimeZone -> FilePath -> IO [ (FilePath,UTCTime) ]
getFilesIO tz = getFiles lsdir where

  lsdir dir = do
    putLog $ "directory : " ++ dir ++ " ... "
    contents <- liftIO $ getDirectoryContents dir
    let valid n = notElem n [".", ".."]
        isOk  n = elem (map toLower $ takeExtension n) [".jpg",".jpeg"]

    stats <- mapM (getStats . (dir </>) ) (filter valid contents)

    let directories = [p | Just (p, stat) <- stats, isDirectory stat]
        files       = [(p,stat) | Just (p, stat) <- stats, isOk p,isRegularFile stat]

    exifs <- mapM (getTime tz) files

    putLog $ show ( length files) ++ " files, " ++ show ( length directories) ++ " directories."
    return (exifs, directories)

-- | Associates a filename with its IO status
-- The status can tell us wether the file is a directory or a regular file,
-- its size, modification date and time, etc.

getStats :: FilePath -> IO (Maybe (FilePath, FileStatus))
getStats path = liftIO $  handle (\(SomeException _) -> return Nothing) $ do
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
groupPhoto s = (map.map) simplify . groupBy ok
  where
    ok _ (_,_,i) = i < s
    simplify (a,b,_) = (a,b)

copyGroup :: TimeZone
          -> FilePath
          -> [(FilePath,UTCTime)]
          -> IO ()
copyGroup _ _ [] = return ()
copyGroup tz root l = do
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
  putLog $ "First file : " ++ (fst $ head l ) ++ " " ++ ds ++ "..."
  putLog $ "Checking directory " ++ ydir ++ "..."
  ok <- liftIO $ doesDirectoryExist ydir
  if ok then putLog "ok."
        else do liftIO $ notDryRun $ createDirectory ydir
                putLog "created."

  -- Checks if the day directory exists, or give it a number
  ddir' <- checkOrCreateDir ddir

  let copy (file,utime) = do
      let dst = replaceDirectory file ddir'
      putLog $ "copying " ++ file ++ " to " ++ dst
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

checkOrCreateDir :: FilePath -> IO FilePath
checkOrCreateDir path = go (1::Int)
  where
    go n = do
      let path' = path ++ if n > 1 then ' ' : show n else ""
      putLog $ "Checking directory " ++ path' ++ "..."
      ok <- liftIO $ doesDirectoryExist path'
      if ok then do putLog "exists."
                    go (n+1)
            else do liftIO $ notDryRun $ createDirectory path'
                    putLog "created."
                    return path'



aff :: (LocalTime, [FilePath]) -> IO ()
aff (a,b) = putStrLn $ show a ++ "  " ++ show ( length b) ++ " photo(s)."


-- test3 :: IO [(String, String)]
-- test3 = Exif.fromFile "/data/perso/media/imatrier/img_1525.jpg" >>= Exif.allTags

-- test4 :: IO ()
-- test4 = do
--   opts <- mkOpts putStr False 7000
--   run opts "/data/media/photos_a_trier/dcimZzzZZ"
--            "/data/media/testphotos"





-- import Graphics.UI.WX
-- 
-- main :: IO ()
-- main = start $
--  do
--   f  <- frame   [ fontSize   := 18
--                 , clientSize := sz 200 200                ]
-- 
--   ok0 <- button f [ on command := close f
--                   , text       := "Close"                  ]
--   ok1 <- button f [ text       := "Remove me"                  ]
-- 
--   set ok1 [ on command :=  do set ok1 [ visible := False ]
--             >                 set f   [ layout  := column 1 [ widget ok0 ] ]
--           ]
-- 
--   set f [ layout :=            column 1 [ widget ok1
--                      , widget ok0
--                      ]
--         ]
-- 
--   return ()