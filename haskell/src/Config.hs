{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
module Config (decodeConfig, Config, Config(..)) where

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
import System.Envy
import Servant.Client
import Servant.Types.SourceT (foreach)
import Text.Read
import Web.JWT
import System.Directory
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX
import Data.String

import Errors

import qualified Servant.Client.Streaming as S

decodeConfig :: IO (Either ConfigError Config)
decodeConfig = fmap (first ConfigError) decodeEnv

data Config = Config 
  { spreadsheetId :: String
  , rsaPrivateKey :: PrivateKey
  , serviceAccountEmail :: StringOrURI
  , audClaim :: StringOrURI
  , expirySeconds :: Integer } deriving (Show, Generic)

instance FromEnv Config

instance Var PrivateKey where toVar = show; fromVar = readRsaSecret . fromString
instance Var StringOrURI where toVar = show; fromVar = stringOrURI . T.pack
