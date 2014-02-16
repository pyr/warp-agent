{-# LANGUAGE OverloadedStrings #-}
module System.Fleet.Payload where
import Control.Applicative
import Data.Aeson
import System.Fleet.Types
import Data.HashMap.Strict (member)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

instance FromJSON Matcher where
  parseJSON (String "all") = pure MatchAll
  parseJSON (Object o) | member "or" o    = MatchOr <$> o .: "or"
                       | member "and" o   = MatchAnd <$> o .: "and"
                       | member "not" o   = MatchNot <$> o .: "not"
                       | member "host" o  = MatchHost <$> o .: "host"
                       | member "fact" o = MatchFact <$> o .: "fact"
                                                     <*> o .: "value"
                       | otherwise = empty
  parseJSON _ = fail "invalid matcher"

instance FromJSON ServiceAction where
  parseJSON (String action) = case action of
                                   "start" -> pure ServiceStart
                                   "restart" -> pure ServiceRestart
                                   "stop" -> pure ServiceStop
                                   "status" -> pure ServiceStatus

instance FromJSON Command where
  parseJSON (String "ping") = pure PingCommand
  parseJSON (Object o) | member "exits" o = ShCommand <$> o .: "shell"
                                                      <*> o .: "exits"
                       | member "shell" o = ShCommand <$> o .: "shell"
                                                      <*> pure [0]
                       | member "sleep" o = SleepCommand <$> o .: "sleep"
                       | member "service" o = ServiceCommand <$> o .: "action"
                                                             <*> o .: "service"
                       | otherwise = empty

instance FromJSON Request where
  parseJSON (Object o) = Request <$> o .: "id"
                                 <*> o .: "match"
                                 <*> o .: "timeout"
                                 <*> o .: "script_name"
                                 <*> o .: "script"
  parseJSON _ = fail "Failed to parse request!"

instance ToJSON CommandOutput where
  toJSON (CommandSuccess code out err) =
    object [ "status" .= String "success",
             "code"   .= code,
             "stdout" .= out,
             "stderr" .= err]
  toJSON (CommandFailure code out err) =
    object [ "status" .= String "failure",
             "code"   .= code,
             "stdout" .= out,
             "stderr" .= err]
  toJSON CommandFinished =
    object [ "status" .= String "finished" ]

instance ToJSON Response where
  toJSON Response{ res_id     = id
                 , res_host   = host
                 , res_output = output } =
    object [ "id"     .= id,
             "host"   .= host,
             "output" .= output ]

instance ToJSON AckStatus where
  toJSON AckStart   = String "starting"
  toJSON AckRefused = String "refused"

instance ToJSON Ack where
  toJSON Ack{ack_id = id, ack_host = host, ack_status = status } =
    object [ "id" .= id, "host" .= host, "status" .= status ]
