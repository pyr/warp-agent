module System.Fleet.Types where
import Data.Map(Map)

type Facts = Map String String

type GenId = String

data ServiceAction = ServiceStop |
                     ServiceStart |
                     ServiceRestart |
                     ServiceStatus deriving (Show, Read)

data Command = PingCommand |
               SleepCommand Integer |
               ShCommand String |
               ServiceCommand ServiceAction String deriving (Show, Read)

data Script = Script String [Command]

data Matcher = MatchAll |
               MatchHost String |
               MatchFact String String |
               MatchNot Matcher |
               MatchOr [Matcher] |
               MatchAnd [Matcher] deriving (Show, Read)

data Request = Request { rq_id         :: GenId
                       , rq_match      :: Matcher
                       , rq_timeout    :: Integer
                       , rq_scriptname :: String
                       , rq_script     :: [Command]
                       } deriving (Show, Read)

data Response = Response { res_id     :: GenId
                         , res_host   :: String
                         , res_output :: [(Int, String, String)]
                         } deriving (Show, Read)

data AckStatus = AckStart |
                 AckRefused deriving (Show, Read)

data Ack = Ack { ack_id     :: GenId
               , ack_host   :: String
               , ack_status :: AckStatus
               } deriving (Show, Read)
