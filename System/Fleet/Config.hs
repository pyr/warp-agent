module System.Fleet.Config where
import System.Fleet.Types
import System.Fleet.Payload
import System.FilePath.Posix
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A

makeAbsolutePaths :: (Monad m) => FilePath -> (m FleetConfig) -> (m FleetConfig)
makeAbsolutePaths from config = do
  cfg <- config
  return $ cfg { cacert = takeDirectory from </> cacert cfg
               , privkey = takeDirectory from </> privkey cfg }

configure :: String -> IO (Either String FleetConfig)
configure path = do
  content <- BL.readFile path
  return $ makeAbsolutePaths path $ A.eitherDecode content
