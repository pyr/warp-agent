{-# LANGUAGE OverloadedStrings #-}
module System.Warp.Agent where
import System.Warp.Types
import System.Warp.Payload
import System.Warp.Match
import System.Warp.Facts
import System.Warp.Executor
import System.Warp.Signature
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
  debugM "Warp.Agent" $ "Request " ++ (show rid) ++
    case output of
     CommandFinished      -> " is finished"
     CommandSuccess _ _ _ -> " was successfully executed"
     CommandFailure e _ _ -> " has failed (" ++ (show e) ++ ")"
  let resp = Response { res_id = rid
                      , res_host = host
                      , res_output = output}
  let resdata = B.concat $ BL.toChunks $ A.encode resp
  sig <- sign_payload privkey $ BC.unpack resdata
  runRedis conn $ publish (BC.pack $ ("warp:res:" ++ rid ++ ":" ++ sig)) resdata
  return ()

redis_ack conn rid privkey payload = do
  let jdata = B.concat $ BL.toChunks $ A.encode payload
  sig <- sign_payload privkey $ BC.unpack jdata
  let chan = "warp:ack:" ++ (rid :: String) ++ sig
  runRedis conn $ publish (BC.pack chan) jdata
  return mempty

redis_run_request conn host privkey req = do
  let Request { rq_id = rid, rq_script = script } = req
  let ack_payload = Ack { ack_id = rid
                        , ack_host = host
                        , ack_status = AckStart }
  let jdata = B.concat $ BL.toChunks $ A.encode ack_payload
  sig <- sign_payload privkey $ BC.unpack jdata
  runRedis conn $ publish (BC.pack $ ("warp:ack:" ++ rid ++ ":" ++ sig)) jdata
  runScript script (redis_publish conn rid host privkey)
  return mempty


redis_match_request conn host box req privkey = do
  debugM "Warp.Agent" $ "Check if request " ++ (show req) ++ " should be executed"
  let Request { rq_match = matcher, rq_id = rid } = req
  facts <- takeMVar box
  let valid_req = runMatch host facts matcher
  putMVar box facts
  if valid_req then do
    debugM "Warp.Agent" $ "Execute request ID " ++ (show rid)
    redis_run_request conn host privkey req
  else do
    debugM "Warp.Agent" $ "Request ID " ++ (show rid) ++ " is not for me"
    redis_ack conn rid privkey (Ack { ack_id = rid
                                    , ack_host = host
                                    , ack_status = AckRefused})


redis_msg conn host box cacert privkey payload = do
  let chan = msgChannel payload
  debugM "Warp.Agent" $ "Got new message on channel: " ++ (BC.unpack chan)
  let sig = last $ splitOn ":" $ BC.unpack chan
  valid <- verify_payload cacert (BC.unpack $ msgMessage payload) sig
  case valid of
    True -> do
      debugM "Warp.Agent" $ "Signature is valid"
      let to_decode = BL.fromChunks [(msgMessage payload)]
      let msg = (A.decode $ to_decode) :: Maybe Request
      case msg of
        Nothing -> do
          warningM "Warp.Agent" $ "Invalid JSON message received: " ++ (show to_decode)
          return ()
        Just req -> redis_match_request conn host box req privkey
      return ()
    False -> do
      warningM "Warp.Agent" $ "Got invalid signature on channel: " ++ (BC.unpack chan)
  return mempty

redis_listen :: MVar Facts -> String -> String -> Integer -> String -> String -> IO ()
redis_listen factbox host redis_host redis_portnum cacert privkey = do
  let redis_port = (PortNumber (fromIntegral redis_portnum))
  let (ConnInfo _ _ auth db max_conn _) = defaultConnectInfo
  let ci = (ConnInfo redis_host redis_port auth db max_conn 600)
  infoM "Warp.Agent" $ "Connect to redis host " ++ redis_host ++ ":" ++ (show redis_portnum)
  cx <- connect ci
  runRedis cx $ pubSub (psubscribe ["warpreq:*"]) (redis_msg cx host factbox cacert privkey)
