module Main where

import Photocop

import System.Environment(getArgs)
import Data.Time.Clock

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
    [ t,src,dst ] -> main' (read t::Int) src dst
    [ src,dst ]   -> main' 7000 src dst
    [ src ]       -> main' 7000 src "/home/david/media/photos"
    []            -> putStrLn "donner au moins le dossier source !"

main' thresh_int srcdir dstdir = do
  let threshold = fromInteger $ toInteger thresh_int :: NominalDiffTime
      logUI _ t = putStrLn t

  photocop logUI threshold srcdir dstdir
