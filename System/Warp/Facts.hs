{-# LANGUAGE OverloadedStrings #-}
module System.Warp.Facts(fetchFacts, startFactThread) where
import System.Warp.Types
import System.Process
import Control.Monad(forever)
import Control.Concurrent.MVar
import Control.Concurrent(forkIO, threadDelay)
import Data.List(intersperse, isInfixOf)
import Data.Map(fromList)
import System.Log.Logger(infoM,debugM)

toFact (fname:(_:fdata)) = (fname,(concat $ intersperse " " fdata))

fetchFacts :: IO (Facts)
fetchFacts = do
  output <- readProcess "facter" [] []
  let facts = [toFact (words l) | l <- lines output, isInfixOf "=>" l]
  debugM "Warp.Facts" $ "Got " ++ show (length facts) ++ " facts"
  return (fromList facts)

startFactThread :: IO (MVar Facts)
startFactThread = do
  box <- newEmptyMVar
  debugM "Warp.Facts" "Retrieve initial facts"
  facts <- fetchFacts
  putMVar box facts
  infoM "Warp.Facts" "Starting facter thread"
  forkIO $ forever $ do
    threadDelay (60 * 1000000)
    facts <- fetchFacts
    -- try to reduce the locking window to a minimum
    _ <- takeMVar box
    putMVar box facts
  return box
