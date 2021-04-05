{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
module Google where

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

import Config
import Errors

type MyMonad m = ExceptT GoogleError m

type API = "v4" :> "spreadsheets" :> Capture "spreadsheetId" String :> "values" :> Capture "range" String :> Header "Authorization" String :> Get '[JSON] Values

data Values = Cells
  { range          :: String
  , majorDimension :: String
  , values         :: [[String]]
  } deriving (Generic, Show)

instance ToJSON Values where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Values

data Cell = Cell { row :: Integer, col :: Integer }

parsePosixTime :: POSIXTime -> Either GoogleError NumericDate
parsePosixTime d = maybeToRight InvalidNumericDate $ numericDate d

epochSeconds :: (MonadError GoogleError m, MonadIO m, Monad m) => m NumericDate
epochSeconds = liftIO getPOSIXTime >>= (liftEither . parsePosixTime)

generateClaims :: (MonadIO m, MonadError GoogleError m) => Config -> m JWTClaimsSet
generateClaims (Config _ _ email audClaim expirySeconds) = do
  now <- epochSeconds
  tokenExpiry <- liftEither $ maybeToRight InvalidExp $ addSeconds expirySeconds now
  return $ constructClaims email audClaim tokenExpiry now otherClaims

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

data GogEnv = GogEnv 
  { config :: Config
  , lastRefreshedJwt :: Integer
  , jwt :: T.Text }

refreshJwt :: (MonadIO m, MonadError GoogleError m) => GogEnv -> m GogEnv
refreshJwt env@(GogEnv config@(Config _ _ _ _ expirySeconds) lastRefreshedJwt jwt) = do
  now <- epochSeconds
  let nowNominal = secondsSinceEpoch now in
    if nowNominal >= (fromInteger (lastRefreshedJwt + expirySeconds))
      then do 
        claims <- generateClaims config
        return (GogEnv config (round nowNominal) (generateJwt config claims))
      else return env

-- getCells :: Config -> IO Values
-- getCells

parseStringOrUri :: String -> Maybe StringOrURI
parseStringOrUri = stringOrURI . T.pack

generateJwt :: Config -> JWTClaimsSet -> T.Text
generateJwt env = encodeSigned (RSAPrivateKey (rsaPrivateKey env)) mempty

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

-- run :: IO ()
-- run = do
--   result <- runExceptT runM
--   case result of
--     Right () -> print "Program completed successfully"
--     Left e -> print $ "Error in program: " ++ show e

-- getCells :: (MonadIO m, MonadError ) -> 

-- buildGoogleEnvironment :: (MonadError GoogleError m, MonadIO m) => Config -> Manager -> m GogEnv
-- buildGoogleEnvironment config manager =

-- runM :: (MonadError AppError m, MonadIO m) => m ()
-- runM = do
--   manager' <- liftIO $ newManager tlsManagerSettings
--   dir <- liftIO getCurrentDirectory
--   -- text <- liftEither $ maybeToRight InvalidText $ readMaybe theKey
--   -- liftIO $ print ("Read the text " ++ text)
--   config <- liftIO $ decodeConfig >>= (\e -> liftEither $ first ConfigErr e)
--   liftIO $ print ("Config is " <> (show config))
--   claims <- generateClaims config
--   liftIO $ print ("Google env is" ++ show config)
--   liftIO $ print ("Read the claims" ++ show claims)
--   liftIO $ print $ show $ generateJwt config claims
