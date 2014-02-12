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
  parseJSON (String "start") = pure ServiceStart
  parseJSON (String "restart") = pure ServiceRestart
  parseJSON (String "stop") = pure ServiceStop
  parseJSON (String "status") = pure ServiceStatus

instance FromJSON Command where
  parseJSON (String "ping") = pure PingCommand
  parseJSON (Object o) | member "shell" o = ShCommand <$> o .: "shell"
                       | member "sleep" o = SleepCommand <$> o .: "sleep"
                       | member "service" o = ServiceCommand <$> o .: "service"
                                                             <*> o .: "action"
                       | otherwise = empty

instance FromJSON Request where
  parseJSON (Object o) = Request <$> o .: "id"
                                 <*> o .: "match"
                                 <*> o .: "timeout"
                                 <*> o .: "script_name"
                                 <*> o .: "script"
  parseJSON _ = fail "Failed to parse request!"


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
