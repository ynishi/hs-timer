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
someFunc = forever $ eval unit

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
eval (Same s x y) = do
  putStrLn s
  (_, _) <- concurrently (eval x) (eval y)
  return ()

unit =
  After
    "unit"
    (Wait "work" (Beep "end") (13 * 60))
    (Wait "summary" (Beep "end") (5 * 60))
