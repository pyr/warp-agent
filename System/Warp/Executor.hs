module System.Warp.Executor (runScript) where
import System.Warp.Types
import System.Warp.Payload
import System.Process
import System.IO
import System.Posix.IO
import System.Exit
import System.Posix.Directory
import Control.Concurrent
import Data.List(elem)
import qualified Control.Exception as C

serviceAction :: ServiceAction -> String
serviceAction ServiceStop  = "stop"
serviceAction ServiceStart  = "start"
serviceAction ServiceRestart  = "restart"
serviceAction ServiceStatus  = "status"
serviceAction ServiceReload = "reload"

readProcessMixedWithExitCode
    :: FilePath                -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> IO (ExitCode,String)     -- ^ exitcode, stdout, stderr
readProcessMixedWithExitCode cmd args = do
  (p_r, p_w) <- createPipe
  h_r <- fdToHandle p_r
  h_w <- fdToHandle p_w
  (Just inh, _, _, pid) <- createProcess (proc cmd args) {
                                     std_in  = CreatePipe,
                                     std_out = UseHandle h_w,
                                     std_err = UseHandle h_w }
  outMVar <- newEmptyMVar

  -- fork off a thread to start consuming stdout
  out  <- hGetContents h_r
  _ <- forkIO $ C.evaluate (length out) >> putMVar outMVar ()

  -- now write and flush any input
  hClose inh -- done with stdin

  -- wait on the output
  takeMVar outMVar
  hClose h_r

  -- wait on the process
  ex <- waitForProcess pid

  return (ex, out)

runRequestCommand :: Command -> IO (CommandOutput)

runRequestCommand PingCommand = do
  return (CommandSuccess 0 "alive" "")

runRequestCommand (SleepCommand amount) = do
  threadDelay (fromIntegral (amount * 1000000) :: Int)
  return (CommandSuccess 0 "slept well" "")

runRequestCommand (ShCommand script cwd exits) = do
  current_wd <- getWorkingDirectory
  if cwd /= "." then
    changeWorkingDirectory cwd
  else
    do {return () }
  (exit,out) <- readProcessMixedWithExitCode "bash" ["-c", script]
  if cwd /= "." then
    changeWorkingDirectory current_wd
  else
    do {return ()}
  case exit of
    ExitSuccess -> return (CommandSuccess 0 out "")
    (ExitFailure code) -> if (elem code exits) then
                             return (CommandSuccess code out "")
                          else
                            return (CommandFailure code out "")


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
