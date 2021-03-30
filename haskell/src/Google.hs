{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
module Google
    ( run
    ) where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import Servant.Types.SourceT (foreach)

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
  manager' <- newManager tlsManagerSettings
  res <- runClientM queries (mkClientEnv manager' serverLocation)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right values -> do
      print values

