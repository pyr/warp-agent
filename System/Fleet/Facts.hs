{-# LANGUAGE OverloadedStrings #-}
module System.Fleet.Facts(fetchFacts, startFactThread) where
import System.Fleet.Types
import System.Process
import Control.Concurrent.MVar
import Control.Concurrent(forkIO, threadDelay)
import Data.List(intersperse, isInfixOf)
import Data.Map(fromList)

toFact (fname:(_:fdata)) = (fname,(concat $ intersperse " " fdata))

fetchFacts :: IO (Facts)
fetchFacts = do
  output <- readProcess "facter" [] []
  let facts = [toFact (words l) | l <- lines output, isInfixOf "=>" l]
  return (fromList facts)

startFactThread :: IO (MVar Facts)
startFactThread = do
  box <- newEmptyMVar
  facts <- fetchFacts
  putMVar box facts
  forkIO $ do
    _ <- takeMVar box
    facts <- fetchFacts
    putMVar box facts
    threadDelay (60 * 1000000)
  return box
