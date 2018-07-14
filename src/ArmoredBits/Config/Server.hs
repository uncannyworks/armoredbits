{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module ArmoredBits.Config.Server where

--------------------------------------------------------------------------------
import Dhall
import GHC.Generics
import Lens.Micro.Platform
--------------------------------------------------------------------------------

data ServerConfig
  = ServerConfig
  { _serverConfigHost :: String
  , _serverConfigPort :: String
  } deriving (Generic, Show)

makeLenses ''ServerConfig

instance Interpret ServerConfig
