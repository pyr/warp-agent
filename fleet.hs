module Main where
import System.Fleet.Agent
import System.Fleet.Facts
import System.Fleet.Config
import System.Fleet.Types
import Network.HostName
import System.Environment

get_conf_path :: [String] -> String
get_conf_path [] = "/etc/fleet-agent.conf"
get_conf_path (path:_) = path

main :: IO ()
main = do
  args <- getArgs
  config <- configure $ get_conf_path args
  case config of
   Left err -> error $ "Unable to parse configuration: " ++ err
   Right conf -> do
     let (FleetConfig { cacert = cacert
                      , privkey = privkey
                      , redis_host = redis_host
                      , redis_port = redis_port}) = conf

     hostname <- getHostName
     factbox <- startFactThread
     redis_listen factbox hostname redis_host redis_port cacert privkey
