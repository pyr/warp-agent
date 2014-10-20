module System.Warp.Config where
import System.Warp.Types
import System.Warp.Payload
import System.FilePath.Posix
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A

makeAbsolutePaths :: (Monad m) => FilePath -> (m WarpConfig) -> (m WarpConfig)
makeAbsolutePaths from config = do
  cfg <- config
  return $ cfg { cacert = takeDirectory from </> cacert cfg
               , privkey = takeDirectory from </> privkey cfg }

configure :: String -> IO (Either String WarpConfig)
configure path = do
  content <- BL.readFile path
  return $ makeAbsolutePaths path $ A.eitherDecode content
