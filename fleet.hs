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

opts :: ParserInfo FleetArguments
opts = info (parseArgs <**> helper)
  ( fullDesc
    <> progDesc "Execute a worker agent for fleet"
    <> header "fleet-agent" )

main :: IO ()
main = do
  args <- execParser opts
  cfg <- configure $ config args
  case cfg of
   Left err -> error $ "Unable to parse configuration: " ++ err
   Right conf -> do
     let (FleetConfig { cacert = cacert
                      , privkey = privkey
                      , redis_host = redis_host
                      , redis_port = redis_port}) = conf

     hostname <- getHostName
     factbox <- startFactThread
     redis_listen factbox hostname redis_host redis_port cacert privkey
