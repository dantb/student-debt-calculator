{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
module Config (main)
 where

import Prelude hiding (exp)
import Control.Monad.Except
import Crypto.PubKey.RSA
import Data.Aeson
import qualified Data.Aeson as A
import Data.Bifunctor
import Data.Either.Combinators
import Data.Proxy
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import System.Envy
import Servant.Client
import Servant.Types.SourceT (foreach)
import Text.Read
import Web.JWT
import System.Directory
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX
import Data.String

import qualified Servant.Client.Streaming as S


main :: IO ()
main = do 
  env <- decodeConfig
  case env of
    Left error -> print error
    Right config -> print $ show config

decodeConfig :: IO (Either String Config)
decodeConfig = decodeEnv

data Config = Config 
  { spreadsheetId :: String
  , rsaPrivateKey :: PrivateKey
  , serviceAccountEmail :: StringOrURI
  , audClaim :: StringOrURI
  , expirySeconds :: Integer } deriving (Show, Generic)

instance FromEnv Config

instance Var PrivateKey where toVar = show; fromVar = readRsaSecret . fromString
instance Var StringOrURI where toVar = show; fromVar = stringOrURI . T.pack
