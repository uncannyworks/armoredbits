{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module ArmoredBits.Config.Client where

--------------------------------------------------------------------------------
import Dhall
import GHC.Generics
import Lens.Micro.Platform
--------------------------------------------------------------------------------

data ClientConfig
  = ClientConfig
  { _clientConfigHost :: String
  , _clientConfigPort :: String
  } deriving (Generic, Show)

makeLenses ''ClientConfig

instance Interpret ClientConfig
