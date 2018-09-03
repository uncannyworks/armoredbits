{-
    Copyright (C) 2014-2018 Uncanny Works

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

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
