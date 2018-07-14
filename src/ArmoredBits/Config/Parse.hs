{-# LANGUAGE FlexibleContexts #-}

module ArmoredBits.Config.Parse where

--------------------------------------------------------------------------------
import Data.Monoid ((<>))
import Options.Applicative
--------------------------------------------------------------------------------

-- | Options passed in on the command line.
data Options
  = Options
  { optionHost :: String
  , optionPort :: String
  }

options :: Parser Options
options = Options
  <$> strOption
      ( long "host"
     <> short 'h'
     <> help "ip to host on or connect to"
     <> showDefault
     <> value "127.0.0.1"
     <> metavar "HOST"
      )
  <*> strOption
      ( long "port"
     <> short 'p'
     <> help "port to host on or connect to"
     <> showDefault
     <> value "275555"
     <> metavar "PORT"
      )

opts :: ParserInfo Options
opts =
  info options
    ( fullDesc
   <> progDesc "options"
   <> header "options"
    )

getOpts :: IO Options
getOpts = execParser opts
