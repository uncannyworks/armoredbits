{-# LANGUAGE FlexibleContexts #-}

module ArmoredBits.Config.Parse where

--------------------------------------------------------------------------------
import Data.Monoid ((<>))
import Options.Applicative
--------------------------------------------------------------------------------
import ArmoredBits.Types
--------------------------------------------------------------------------------

-- | Options passed in on the command line.
data Options
  = Options
  { optionHost   :: String
  , optionPort   :: String
  , optionTokens :: [Token]
  }

tokens :: Parser [Token]
tokens = many (argument str (metavar "TOKEN..."))

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
     <> value "27555"
     <> metavar "PORT"
      )
  <*> tokens

opts :: ParserInfo Options
opts =
  info (helper <*> options)
    ( fullDesc
   <> progDesc "Start a server that accepts a space separated list of TOKEN..."
   <> header "armoredbits-[server|client]"
    )

getOpts :: IO Options
getOpts = execParser opts
