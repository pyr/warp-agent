module Main where
import System.Fleet.Agent
import System.Fleet.Facts
import System.Fleet.Config
import System.Fleet.Args
import System.Fleet.Types
import Network.HostName
import System.Environment
import Options.Applicative
import Data.List(break)
import Control.Concurrent.MVar (takeMVar, putMVar)
import qualified Data.Map as M

import System.IO (stderr)
import System.Log.Logger (updateGlobalLogger, rootLoggerName,
                          addHandler, removeHandler,
                          setLevel, traplogging,
                          infoM,
                          Priority(DEBUG, INFO, ERROR))
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Formatter (simpleLogFormatter)

opts :: ParserInfo FleetArguments
opts = info (parseArgs <**> helper)
  ( fullDesc
    <> progDesc "Execute a worker agent for fleet"
    <> header "fleet-agent" )

-- Logging configuration
configureLogging :: FleetArguments -> IO ()
configureLogging cfg = do
  h <- streamHandler stderr DEBUG >>=
       \lh -> return $ setFormatter lh (simpleLogFormatter "$time $loggername [$prio] $msg")
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger rootLoggerName $ addHandler h
  configureLoggingVerbosity $ verbosity cfg
  configureLoggingTarget $ logfile cfg

configureLoggingVerbosity :: Verbosity -> IO ()
configureLoggingVerbosity Normal =
  updateGlobalLogger rootLoggerName $ setLevel INFO
configureLoggingVerbosity Verbose =
  updateGlobalLogger rootLoggerName $ setLevel DEBUG

configureLoggingTarget :: (Maybe FilePath) -> IO ()
configureLoggingTarget Nothing = return ()
configureLoggingTarget (Just target) = do
  h <- fileHandler target INFO >>=
       \lh -> return $ setFormatter lh (simpleLogFormatter "$time $loggername [$prio] $msg")
  updateGlobalLogger rootLoggerName $ addHandler h

-- Main
main :: IO ()
main = do
  args <- execParser opts
  configureLogging args
  cfg <- configure $ config args
  case cfg of
   Left err -> error $ "Unable to parse configuration: " ++ err
   Right conf -> do
     let (FleetConfig { cacert = cacert
                      , privkey = privkey
                      , redis_host = redis_host
                      , redis_port = redis_port}) = conf

     traplogging "Fleet.main" ERROR "A fatal exception occurred" $ do
       factbox <- startFactThread
       facts <- takeMVar factbox
       hostname <- return $ M.lookup "fqdn" facts
       putMVar factbox facts
       case hostname of
         Nothing -> error $ "Unable to get FQDN from facts"
         Just fqdn -> do
           infoM "Fleet.main" $ "My hostname is " ++ fqdn
           redis_listen factbox fqdn redis_host redis_port cacert privkey
