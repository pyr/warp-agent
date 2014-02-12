module System.Fleet.Executor (runScript) where
import System.Fleet.Types
import System.Fleet.Payload
import System.Process
import System.Exit
import Control.Concurrent

serviceAction :: ServiceAction -> String
serviceAction ServiceStop  = "stop"
serviceAction ServiceStart  = "start"
serviceAction ServiceRestart  = "restart"
serviceAction ServiceStatus  = "status"

runRequestCommand :: Command -> IO (Int, String, String)

runRequestCommand PingCommand = do
  return (0, "alive", "")

runRequestCommand (SleepCommand amount) = do
  threadDelay (fromIntegral (amount * 1000000) :: Int)
  return (0, "slept well", "")

runRequestCommand (ShCommand script) = do
  (exit,out,err) <- readProcessWithExitCode "bash" ["-c", script] []
  case exit of
    ExitSuccess -> return (0, out, err)
    (ExitFailure code) -> return (code, out, err)

runRequest (ServiceCommand action service) = do
  (exit, out, err) <- readProcessWithExitCode "service"
                      [serviceAction action, service] []
  case exit of
    ExitSuccess -> return (0, out, err)
    (ExitFailure code) -> return (code, out, err)


runScript :: [Command] -> IO ([(Int, String, String)])

runScript [] = do
  return []

runScript (cmd:commands) = do
  output <- runRequestCommand cmd
  let (code, _, _)  = output
  if code == 0 then do
      next <- runScript commands
      return (output:next)
  else
    return [output]
