module System.Fleet.Config where
import System.Fleet.Types
import System.Fleet.Payload
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A

configure :: String -> IO (FleetConfig)
configure path = do
  content <- BL.readFile path
  let (Just config) = A.decode content :: Maybe FleetConfig
  return config
