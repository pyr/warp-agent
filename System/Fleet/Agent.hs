{-# LANGUAGE OverloadedStrings #-}
module System.Fleet.Agent where
import System.Fleet.Types
import System.Fleet.Payload
import System.Fleet.Match
import System.Fleet.Facts
import System.Fleet.Executor
import Database.Redis
import Data.Monoid (mempty)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import qualified Data.Map as M

redis_ack conn rid payload = do
  let jdata = B.concat $ BL.toChunks $ A.encode payload
  let chan = "fleet:ack:" ++ (rid :: String)
  runRedis conn $ publish (BC.pack chan) jdata
  return mempty

redis_run_request conn host req = do
  let Request { rq_id = rid, rq_script = script } = req
  let ack_payload = Ack { ack_id = rid
                        , ack_host = host
                        , ack_status = AckStart }
  let jdata = B.concat $ BL.toChunks $ A.encode ack_payload
  runRedis conn $ publish (BC.pack $ ("fleet:ack:" ++ rid)) jdata
  output <- runScript script
  let resp = Response { res_id = rid
                      , res_host = host
                      , res_output = output }
  let resdata = B.concat $ BL.toChunks $ A.encode resp
  runRedis conn $ publish (BC.pack $ ("fleet:res:" ++ rid)) resdata
  return ()


redis_match_request conn host box req = do
  let Request { rq_match = matcher, rq_id = rid } = req
  facts <- takeMVar box
  let valid_req = runMatch host facts matcher
  putMVar box facts
  if valid_req then
    redis_run_request conn host req
  else
    redis_ack conn rid (Ack { ack_id = rid
                           , ack_host = host
                           , ack_status = AckRefused})


redis_msg conn host box payload = do
  let to_decode = BL.fromChunks [(msgMessage payload)]
  let msg = (A.decode $ to_decode) :: Maybe Request
  putStrLn $ show msg
  case msg of
    Nothing -> do { return ()}
    Just req -> redis_match_request conn host box req
  return mempty

redis_listen :: MVar Facts -> String -> String -> Integer -> IO ()
redis_listen factbox host redis_host redis_portnum = do
  let redis_port = (PortNumber (fromIntegral redis_portnum))
  let (ConnInfo _ _ auth max_conn max_idle) = defaultConnectInfo
  let ci = (ConnInfo redis_host redis_port auth max_conn max_idle)
  cx <- connect ci
  runRedis cx $ pubSub (psubscribe ["fleetreq:*"]) (redis_msg cx host factbox)

test = do
  box <- newEmptyMVar
  facts <- fetchFacts
  putMVar box facts
  redis_listen box "phoenix" "localhost" 6379
