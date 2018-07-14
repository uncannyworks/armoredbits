{-|

Module      : ArmoredBits.Util
Description : Various minor utility functions that come up

|-}
module ArmoredBits.Util where

--------------------------------------------------------------------------------
-- Time conversion for things like `threadDelay`

inMilliseconds :: Int -> Int
inMilliseconds = (* 1000)

inSeconds :: Int -> Int
inSeconds = (* 1000000)

--------------------------------------------------------------------------------
