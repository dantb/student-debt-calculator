module Errors where

import Servant.Client

data AppError = ConfigErr ConfigError | GoogleErr GoogleError | ServantError

data GoogleError = InvalidExp | InvalidNumericDate deriving Show

newtype ConfigError = ConfigError String
