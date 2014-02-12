module Main where
import System.Fleet.Agent
import System.Fleet.Facts
import Network.HostName
import System.Environment
import Data.List(break)

main :: IO ()
main = do
  (redis_info:_) <- getArgs
  let (redis_host, redis_str_port) = break (== ':') redis_info
  let redis_port = if redis_str_port == "" then
                     6379
                   else
                     (read (tail redis_str_port) :: Integer)
  hostname <- getHostName
  let (shortname,_) = break (== '.') hostname
  factbox <- startFactThread
  redis_listen factbox hostname redis_host redis_port
