{-# LANGUAGE OverloadedStrings #-}
module System.Fleet.Agent where
import System.Fleet.Types
import System.Fleet.Payload
import System.Fleet.Match
import System.Fleet.Facts
import System.Fleet.Executor
import System.Fleet.Signature
import Database.Redis
import Data.List.Split (splitOn)
import Data.Monoid (mempty)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import qualified Data.Map as M

redis_publish conn rid host privkey output = do
  let resp = Response { res_id = rid
                      , res_host = host
                      , res_output = output}
  let resdata = B.concat $ BL.toChunks $ A.encode resp
  sig <- sign_payload privkey $ BC.unpack resdata
  runRedis conn $ publish (BC.pack $ ("fleet:res:" ++ rid ++ ":" ++ sig)) resdata
  return ()

redis_ack conn rid privkey payload = do
  let jdata = B.concat $ BL.toChunks $ A.encode payload
  sig <- sign_payload privkey $ BC.unpack jdata
  let chan = "fleet:ack:" ++ (rid :: String) ++ sig
  runRedis conn $ publish (BC.pack chan) jdata
  return mempty

redis_run_request conn host privkey req = do
  let Request { rq_id = rid, rq_script = script } = req
  let ack_payload = Ack { ack_id = rid
                        , ack_host = host
                        , ack_status = AckStart }
  let jdata = B.concat $ BL.toChunks $ A.encode ack_payload
  sig <- sign_payload privkey $ BC.unpack jdata
  runRedis conn $ publish (BC.pack $ ("fleet:ack:" ++ rid ++ ":" ++ sig)) jdata
  runScript script (redis_publish conn rid host privkey)
  return mempty


redis_match_request conn host box req privkey = do
  let Request { rq_match = matcher, rq_id = rid } = req
  facts <- takeMVar box
  let valid_req = runMatch host facts matcher
  putMVar box facts
  if valid_req then
    redis_run_request conn host privkey req
  else
    redis_ack conn rid privkey (Ack { ack_id = rid
                                    , ack_host = host
                                    , ack_status = AckRefused})


redis_msg conn host box cacert privkey payload = do
  let chan = msgChannel payload
  let sig = last $ splitOn ":" $ BC.unpack chan
  valid <- verify_payload cacert (BC.unpack $ msgMessage payload) sig
  case valid of
    True -> do
      let to_decode = BL.fromChunks [(msgMessage payload)]
      let msg = (A.decode $ to_decode) :: Maybe Request
      case msg of
        Nothing -> do { return ()}
        Just req -> redis_match_request conn host box req privkey
      return ()
    False -> do
      putStrLn $ "got bad message on channel: " ++ (BC.unpack chan)
  return mempty

redis_listen :: MVar Facts -> String -> String -> Integer -> String -> String -> IO ()
redis_listen factbox host redis_host redis_portnum cacert privkey = do
  let redis_port = (PortNumber (fromIntegral redis_portnum))
  let (ConnInfo _ _ auth db max_conn max_idle) = defaultConnectInfo
  let ci = (ConnInfo redis_host redis_port auth db max_conn max_idle)
  cx <- connect ci
  runRedis cx $ pubSub (psubscribe ["fleetreq:*"]) (redis_msg cx host factbox cacert privkey)
