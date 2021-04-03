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

import qualified Servant.Client.Streaming as S

data Values = Cells
  { range          :: String
  , majorDimension :: String
  , values         :: [[String]]
  } deriving (Generic, Show)

instance ToJSON Values where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Values

-- $(deriveJSON defaultOptions ''Values)

data Cell = Cell { row :: Integer, col :: Integer }

data GoogleEnvironment = GoogleEnvironment { spreadsheetId :: String, privateKey :: PrivateKey }

claimsSet2 :: Either InvalidClaim JWTClaimsSet
claimsSet2 = do
  iss <- maybeToRight InvalidIss $ parseStringOrUri "student-debt-projection-sa@student-debt-projection.iam.gserviceaccount.com"
  aud <- maybeToRight InvalidAud $ parseStringOrUri "https://oauth2.googleapis.com/token"
  exp <- maybeToRight InvalidExp $ numericDate 1617484700
  iat <- maybeToRight InvalidIat $ numericDate 1617483700
  return (claimsSet3 iss aud exp iat (ClaimsMap $ M.singleton (T.pack "scope") (A.String (T.pack "https://www.googleapis.com/auth/spreadsheets"))))

claimsSet3 :: StringOrURI -> StringOrURI -> NumericDate -> NumericDate -> ClaimsMap -> JWTClaimsSet
claimsSet3 iss aud exp iat claims = JWTClaimsSet 
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
generateJwt env claims = encodeSigned (RSAPrivateKey (privateKey env)) mempty claims

readKey :: IO (Maybe PrivateKey)
readKey = fmap (parseKey) (readFile "./scripts/private.pem")

parseKey :: String -> Maybe PrivateKey
parseKey t = (readMaybe t) >>= readRsaSecret

anotherOne :: IO (Maybe PrivateKey)
anotherOne = readRsaSecret <$> BS.readFile "./scripts/private.pem"

type API = "v4" :> "spreadsheets" :> Capture "spreadsheetId" String :> "values" :> Capture "range" String :> Header "Authorization" String :> Get '[JSON] Values

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
  -- res <- runClientM queries (mkClientEnv manager' serverLocation)
  -- case res of
  --   Left err -> putStrLn $ "Error: " ++ show err
  --   Right values -> do
  --     print values

data InvalidClaim = InvalidIss | InvalidAud | InvalidExp | InvalidIat deriving Show

data GoogleError = InvalidClaim InvalidClaim | InvalidPrivateKey | InvalidText deriving Show

type MyMonad m = ExceptT GoogleError m

runM :: MonadIO m => MyMonad m ()
runM = do
  manager' <- liftIO $ newManager tlsManagerSettings
  dir <- liftIO getCurrentDirectory
  -- text <- liftEither $ maybeToRight InvalidText $ readMaybe theKey
  -- liftIO $ print ("Read the text " ++ text)
  liftIO $ print ("Current directory is " ++ dir)
  key <- ExceptT $ liftIO $ fmap (maybeToRight InvalidPrivateKey) anotherOne
  liftIO $ print "Read the key"
  claims <- liftEither $ first InvalidClaim claimsSet2
  liftIO $ print ("Read the claims" ++ show claims)
  liftIO $ print $ show $ generateJwt (GoogleEnvironment "1nCeGC0XHnIVwQ3EyGRMZRmExkiP_wpVgotIZQS98e28" key) claims
