{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
module Google
    ( run
    ) where

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
import Servant.Client
import Servant.Types.SourceT (foreach)
import Text.Read
import Web.JWT
import System.Directory
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX

import qualified Servant.Client.Streaming as S

type MyMonad m = ExceptT GoogleError m

type API = "v4" :> "spreadsheets" :> Capture "spreadsheetId" String :> "values" :> Capture "range" String :> Header "Authorization" String :> Get '[JSON] Values

data InvalidClaim = InvalidIss | InvalidAud | InvalidExp | InvalidIat | InvalidNumericDate deriving Show

data GoogleError = InvalidClaim InvalidClaim | InvalidPrivateKey | InvalidText deriving Show

data Values = Cells
  { range          :: String
  , majorDimension :: String
  , values         :: [[String]]
  } deriving (Generic, Show)

instance ToJSON Values where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Values

data Cell = Cell { row :: Integer, col :: Integer }

data GoogleEnvironment = GoogleEnvironment 
  { spreadsheetId :: String
  , privateKey :: PrivateKey
  , serviceAccountEmail :: StringOrURI
  , audClaim :: StringOrURI
  , expirySeconds :: Integer }
  deriving Show

epochSeconds :: IO POSIXTime
epochSeconds = getPOSIXTime

parsePosixTime :: POSIXTime -> Either InvalidClaim NumericDate
parsePosixTime d = maybeToRight InvalidNumericDate $ numericDate d

generateClaims :: MonadIO m => GoogleEnvironment -> MyMonad m JWTClaimsSet
generateClaims (GoogleEnvironment _ _ email audClaim expirySeconds) = do
  now <- liftIO epochSeconds
  nowNumericDate <- liftEither $ first InvalidClaim (parsePosixTime now)
  tokenExpiry <- liftEither $ maybeToRight (InvalidClaim InvalidExp) $ addSeconds expirySeconds nowNumericDate
  return $ constructClaims email audClaim tokenExpiry nowNumericDate otherClaims

addSeconds :: Integer -> NumericDate -> Maybe NumericDate
addSeconds i n = numericDate $ secondsSinceEpoch n + fromInteger i

otherClaims :: ClaimsMap
otherClaims = ClaimsMap $ M.singleton (T.pack "scope") (A.String (T.pack "https://www.googleapis.com/auth/spreadsheets"))

constructClaims :: StringOrURI -> StringOrURI -> NumericDate -> NumericDate -> ClaimsMap -> JWTClaimsSet
constructClaims iss aud exp iat claims = JWTClaimsSet 
  { iss =  Just iss
  , sub = Nothing
  , aud = Just $ Left aud
  , exp = Just exp
  , nbf = Nothing 
  , iat = Just iat
  , jti = Nothing
  , unregisteredClaims = claims
  }

parseStringOrUri :: String -> Maybe StringOrURI
parseStringOrUri = stringOrURI . T.pack

generateJwt :: GoogleEnvironment -> JWTClaimsSet -> T.Text
generateJwt env = encodeSigned (RSAPrivateKey (privateKey env)) mempty

readKey :: IO (Maybe PrivateKey)
readKey = readRsaSecret <$> BS.readFile "./scripts/private.pem"

parseKey :: String -> Maybe PrivateKey
parseKey t = readMaybe t >>= readRsaSecret

sheetValues :: String -> String -> Maybe String -> ClientM Values
sheetValues = client api

api :: Proxy API
api = Proxy

queries :: ClientM Values
queries =
  sheetValues "1nCeGC0XHnIVwQ3EyGRMZRmExkiP_wpVgotIZQS98e28" "Sheet1!B5:E5" (Just "Bearer ya29.c.Kp8B-QfSj2N2znAdXAMJCs6_5ETXuAnakKgLwwxRZdll0w-Ac7QstygVZi0dGzWrvf_gV9o_vdPeHyWYkZ3xrnnRGqrX40Oixiqk9TyYAgXM5N9QTGEGLBNwCSQR9gUtoUfXXg2QQLFIhDgAn2ssysG6Du3mTuXHN3gbaKDVtgdTtmUh9ODw1JPCHGBRkQdjabF-xr-X1IfLLpbCVl_avkkk")

serverLocation :: BaseUrl
serverLocation = BaseUrl
  { baseUrlScheme = Https
  , baseUrlHost   = "sheets.googleapis.com"
  , baseUrlPort   = 443
  , baseUrlPath   = ""
  }

run :: IO ()
run = do
  result <- runExceptT runM
  case result of
    Right () -> print "Program completed successfully"
    Left e -> print $ "Error in program: " ++ show e

runM :: MonadIO m => MyMonad m ()
runM = do
  manager' <- liftIO $ newManager tlsManagerSettings
  dir <- liftIO getCurrentDirectory
  -- text <- liftEither $ maybeToRight InvalidText $ readMaybe theKey
  -- liftIO $ print ("Read the text " ++ text)
  liftIO $ print ("Current directory is " ++ dir)
  key <- ExceptT $ liftIO $ fmap (maybeToRight InvalidPrivateKey) readKey
  liftIO $ print "Read the key"
  email <- liftEither $ maybeToRight (InvalidClaim InvalidIss) $ parseStringOrUri "student-debt-projection-sa@student-debt-projection.iam.gserviceaccount.com"
  audClaim <- liftEither $ maybeToRight (InvalidClaim InvalidAud) $ parseStringOrUri "https://oauth2.googleapis.com/token"
  let googleEnv = GoogleEnvironment "1nCeGC0XHnIVwQ3EyGRMZRmExkiP_wpVgotIZQS98e28" key email audClaim 1000
  claims <- generateClaims googleEnv
  liftIO $ print ("Google env is" ++ show googleEnv)
  liftIO $ print ("Read the claims" ++ show claims)
  liftIO $ print $ show $ generateJwt googleEnv claims
