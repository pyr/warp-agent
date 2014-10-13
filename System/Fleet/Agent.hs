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
import System.Log.Logger (infoM, debugM, warningM)

redis_publish conn rid host privkey output = do
  debugM "Fleet.Agent" $ "Request " ++ (show rid) ++
    case output of
     CommandFinished      -> " is finished"
     CommandSuccess _ _ _ -> " was successfully executed"
     CommandFailure e _ _ -> " has failed (" ++ (show e) ++ ")"
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
  debugM "Fleet.Agent" $ "Check if request " ++ (show req) ++ " should be executed"
  let Request { rq_match = matcher, rq_id = rid } = req
  facts <- takeMVar box
  let valid_req = runMatch host facts matcher
  putMVar box facts
  if valid_req then do
    debugM "Fleet.Agent" $ "Execute request ID " ++ (show rid)
    redis_run_request conn host privkey req
  else do
    debugM "Fleet.Agent" $ "Request ID " ++ (show rid) ++ " is not for me"
    redis_ack conn rid privkey (Ack { ack_id = rid
                                    , ack_host = host
                                    , ack_status = AckRefused})


redis_msg conn host box cacert privkey payload = do
  let chan = msgChannel payload
  debugM "Fleet.Agent" $ "Got new message on channel: " ++ (BC.unpack chan)
  let sig = last $ splitOn ":" $ BC.unpack chan
  valid <- verify_payload cacert (BC.unpack $ msgMessage payload) sig
  case valid of
    True -> do
      debugM "Fleet.Agent" $ "Signature is valid"
      let to_decode = BL.fromChunks [(msgMessage payload)]
      let msg = (A.decode $ to_decode) :: Maybe Request
      case msg of
        Nothing -> do
          warningM "Fleet.Agent" $ "Invalid JSON message received: " ++ (show to_decode)
          return ()
        Just req -> redis_match_request conn host box req privkey
      return ()
    False -> do
      warningM "Fleet.Agent" $ "Got invalid signature on channel: " ++ (BC.unpack chan)
  return mempty

redis_listen :: MVar Facts -> String -> String -> Integer -> String -> String -> IO ()
redis_listen factbox host redis_host redis_portnum cacert privkey = do
  let redis_port = (PortNumber (fromIntegral redis_portnum))
  let (ConnInfo _ _ auth db max_conn max_idle) = defaultConnectInfo
  let ci = (ConnInfo redis_host redis_port auth db max_conn max_idle)
  infoM "Fleet.Agent" $ "Connect to redis host " ++ redis_host ++ ":" ++ (show redis_portnum)
  cx1 <- connect ci
  cx2 <- connect ci
  runRedis cx1 $ pubSub (psubscribe ["fleetreq:*"]) (redis_msg cx2 host factbox cacert privkey)
