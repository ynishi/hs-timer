{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  , Timer(..)
  , eval
  , unit
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad            (forM_, forever)
import           Data.Foldable
import           System.IO                (hFlush, stdout)
import           System.Process
import           System.ProgressBar
import           Text.Printf

someFunc :: IO ()
someFunc = forever $ eval oneHour

sec n = n * 1000000

sleep = threadDelay . sec

data Timer
  = Beep String
  | Wait String
         Timer
         Int
  | After String
          Timer
          Timer
  | Repeat String
           Timer
           Int
  | Same String
         Timer
         Timer
  deriving (Show)

eval (Beep s) = do
  putStrLn s
  _ <- readProcess "ktimer" ["0"] ""
  return ()
eval (Wait s t i) = do
  let title = printf "%s: %d min(%d sec)" s (i `div` 60) i
  putStrLn title
  let work = sleep 1
  let toBeDone = replicate i ()
  pb <- newProgressBar defStyle 10 (Progress 0 i ())
  async $ sleep i
  for_ toBeDone $ \() -> do
    work
    incProgress pb 1
  eval t
eval (After s x y) = do
  putStrLn s
  eval x
  eval y
eval (Repeat s t n) = do
  putStrLn s
  for_ (replicate n ()) $ \() -> eval t
eval (Same s x y) = do
  putStrLn s
  (_, _) <- concurrently (eval x) (eval y)
  return ()

wt = 10
rt = 3
ut = wt + rt

wn = 60 `div` ut
nt = 60 `mod` ut

dunit = unit wt rt

unit w r =
  After
    "unit"
    (Wait "work" (Beep "end") (w * 60))
    (Wait "summary" (Beep "end") (r * 60))

oneHour = After "oneHour" (Repeat ((show wn) ++ " unit") dunit wn) (Wait "rest" (Beep "end") (nt * 60))
