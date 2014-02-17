module System.Fleet.Executor (runScript) where
import System.Fleet.Types
import System.Fleet.Payload
import System.Process
import System.Exit
import Control.Concurrent
import Data.List(elem)

serviceAction :: ServiceAction -> String
serviceAction ServiceStop  = "stop"
serviceAction ServiceStart  = "start"
serviceAction ServiceRestart  = "restart"
serviceAction ServiceStatus  = "status"

runRequestCommand :: Command -> IO (CommandOutput)

runRequestCommand PingCommand = do
  return (CommandSuccess 0 "alive" "")

runRequestCommand (SleepCommand amount) = do
  threadDelay (fromIntegral (amount * 1000000) :: Int)
  return (CommandSuccess 0 "slept well" "")

runRequestCommand (ShCommand script exits) = do
  (exit,out,err) <- readProcessWithExitCode "bash" ["-c", script] []
  case exit of
    ExitSuccess -> return (CommandSuccess 0 out err)
    (ExitFailure code) -> if (elem code exits) then
                             return (CommandSuccess code out err)
                          else
                            return (CommandFailure code out err)

runRequestCommand (ServiceCommand action service) = do
  (exit, out, err) <- readProcessWithExitCode "service"
                      [service, serviceAction action] []
  case exit of
    ExitSuccess -> return (CommandSuccess 0 out err)
    (ExitFailure code) -> return (CommandFailure code out err)


runNext :: CommandOutput -> (CommandOutput -> IO ()) -> [Command] -> IO ()

runNext (CommandSuccess code out err) reporter commands = do
  reporter (CommandSuccess code out err)
  runScript commands reporter
  return ()

runNext (CommandFailure code out err) reporter commands = do
  reporter (CommandFailure code out err)

runScript :: [Command] -> (CommandOutput -> IO ()) -> IO ()

runScript [] reporter = do
  reporter CommandFinished

runScript (cmd:commands) reporter = do
  output <- runRequestCommand cmd
  runNext output reporter commands
