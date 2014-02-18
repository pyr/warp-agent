module System.Fleet.Signature where
import Network.TLS.Extra
import Network.TLS
import Codec.Crypto.RSA
import Data.Certificate.X509
import qualified Codec.Binary.Base64.String as B64
import qualified Data.ByteString.Lazy.Char8 as BL

sign_payload :: String -> String -> IO (String)
sign_payload keypath str = do
  (PrivRSA key) <- fileReadPrivateKey keypath
  let payload = BL.pack str
  let signed = sign key payload
  let b64 = concat $ lines $ B64.encode $ BL.unpack signed
  return b64

verify_payload :: String -> String -> String -> IO (Bool)
verify_payload certpath str sig = do
  (X509 {x509Cert = cert}) <- fileReadCertificate certpath
  let (Certificate {certPubKey = pubkey}) = cert
  let (PubKeyRSA pubrsa) = pubkey
  let valid = verify pubrsa (BL.pack str) (BL.pack $ B64.decode sig)
  return valid
