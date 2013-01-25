module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Photocop

-- import System.Directory(copyFile,doesDirectoryExist,createDirectory,getDirectoryContents,getHomeDirectory)
-- import System.Posix.Files(setFileTimes,FileStatus,getFileStatus,modificationTime,isDirectory,isRegularFile)
-- import System.Posix.Types(EpochTime)
-- import System.FilePath((</>),replaceDirectory,takeExtension)
-- import System.Locale(defaultTimeLocale)
import System.Environment(getArgs)
-- import Control.Monad(unless,msum)
-- import Control.Monad.Reader(runReaderT,ReaderT,ask)
-- import Control.Monad.Trans(liftIO)
-- import Control.Monad.Maybe(runMaybeT,MaybeT(..))
-- import Control.Exception(handle,SomeException(..))
-- import Data.Char(toLower)
-- import Data.Ord(comparing)
-- import Data.List(groupBy,sortBy)
-- -- import Data.Function
import Data.Time.Clock
-- import Data.Time.Clock.POSIX(posixSecondsToUTCTime,utcTimeToPOSIXSeconds)
-- import Data.Time.LocalTime(getCurrentTimeZone,LocalTime,utcToLocalTime,TimeZone)
-- import Data.Time.Format(parseTime)
-- import Graphics.Exif as Exif (fromFile,getTag)

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
  let threshold = fromInteger $ toInteger thresh_int :: NominalDiffTime
  dlg      <- frame [ text := "Photocop", clientSize := sz 500 300 ]
  pause    <- button dlg [ text := "Pause" ]
  stop     <- button dlg [ text := "Stop" ]
  progress <- hgauge dlg 100 []
  tick     <- timer dlg [interval := 100]
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
  chan <- atomically $ newTChan
  
  let logUI p t = let f = do
                              putStrLn t
                              set current [ text := t ]
                              set progress [ selection := p ]
                  in atomically $ writeTChan chan f

  set tick [ on command := updateUI chan ]

  forkOS $ do
    photocop logUI threshold srcdir dstdir
    close dlg

-- zz = do
--   putStrLn "wait"
-- 
--   takeMVar wait
--   close dlg

updateUI chan = do
  putStrLn "tick"
  a <- atomically $ do
    e <- isEmptyTChan chan
    if e then return (return ()) else readTChan chan
  a
