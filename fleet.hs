module Main where
import System.Fleet.Agent
import System.Fleet.Facts
import System.Fleet.Config
import System.Fleet.Types
import Network.HostName
import System.Environment
import Data.List(break)

get_conf_path [] = "/etc/fleet-agent.conf"
get_conf_path (path:_) = path

main :: IO ()
main = do
  args <- getArgs
  config <- configure $ get_conf_path args
  let (FleetConfig { cacert = cacert
                   , privkey = privkey
                   , redis_host = redis_host
                   , redis_port = redis_port}) = config

  hostname <- getHostName
  let (shortname,_) = break (== '.') hostname
  factbox <- startFactThread
  redis_listen factbox hostname redis_host redis_port cacert privkey
