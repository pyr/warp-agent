module System.Fleet.Types where
import Data.Map(Map)

type Facts = Map String String

type GenId = String

data ServiceAction = ServiceStop |
                     ServiceStart |
                     ServiceRestart |
                     ServiceReload |
                     ServiceStatus deriving (Show, Read)

data Command = PingCommand |
               SleepCommand Integer |
               ShCommand String String [Int] |
               ServiceCommand ServiceAction String deriving (Show, Read)

data Script = Script String [Command]

data Matcher = MatchAll |
               MatchHost String |
               MatchFact String String |
               MatchNot Matcher |
               MatchOr [Matcher] |
               MatchAnd [Matcher] deriving (Show, Read)

data CommandOutput = CommandSuccess Int String String
                   | CommandFailure Int String String
                   | CommandFinished deriving (Show, Read)

data Request = Request { rq_id         :: GenId
                       , rq_match      :: Matcher
                       , rq_timeout    :: Integer
                       , rq_scriptname :: String
                       , rq_script     :: [Command]
                       } deriving (Show, Read)

data Response = Response { res_id     :: GenId
                         , res_host   :: String
                         , res_output :: CommandOutput
                         } deriving (Show, Read)

data AckStatus = AckStart |
                 AckRefused deriving (Show, Read)

data Ack = Ack { ack_id     :: GenId
               , ack_host   :: String
               , ack_status :: AckStatus
               } deriving (Show, Read)

data FleetConfig = FleetConfig { cacert     :: String
                               , privkey    :: String
                               , redis_host :: String
                               , redis_port :: Integer
                               } deriving (Show, Read)

data Verbosity = Normal | Verbose deriving (Show, Read)
data FleetArguments = FleetArguments { config    :: FilePath
                                     , verbosity :: Verbosity
                                     , logfile   :: Maybe FilePath
                                     } deriving (Show, Read)
