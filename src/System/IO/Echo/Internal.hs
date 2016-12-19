{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 702
# if defined(WINDOWS)
{-# LANGUAGE Trustworthy #-}
# else
{-# LANGUAGE Safe #-}
# endif
#endif

{-|
Module:      System.IO.Echo.Internal
Copyright:   (C) 2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: Portable

Exports functions that handle whether or not terminal input is handled in a way
that should be portable across different platforms and consoles.

Unlike "System.IO.Echo", this module export internal functionality which, if
used improperly, can lead to runtime errors. Make sure to read the
documentation beforehand!
-}
module System.IO.Echo.Internal (
      -- * Safe public interface
      withoutInputEcho, bracketInputEcho
    , getInputEchoState, setInputEchoState
    , echoOff, echoOn

      -- * Alternative (safe) interface
    , getInputEcho, setInputEcho

      -- * Unsafe STTY internals
    , EchoState(..), STTYSettings
    , getInputEchoSTTY, setInputEchoSTTY, sttyRaw

      -- * MinTTY
    , minTTY
    ) where

import Control.Exception (bracket, throw)
import Control.Monad (void)

import Data.List (isInfixOf)

import System.Exit (ExitCode(..))
import System.IO (hGetContents, hGetEcho, hSetEcho, stdin)
import System.Process (StdStream(..), createProcess, shell,
                       std_in, std_out, waitForProcess)

#if defined(WINDOWS)
import Graphics.Win32.Misc (getStdHandle, sTD_INPUT_HANDLE)

import System.IO.Echo.MinTTY (isMinTTYHandle)
import System.IO.Unsafe (unsafePerformIO)
#endif

-- RGS TODO
getInputEcho :: IO Bool
getInputEcho = if minTTY
                  then do settings <- sttyRaw "-a"
                          -- This assumes that other settings come after
                          -- [-]echo in the output of `stty -a`. Luckily, this
                          -- seems to be the case on every incarnation of
                          -- MinTTY that I've tried.
                          return $ not ("-echo " `isInfixOf` settings)
                  else hGetEcho stdin

-- RGS TODO
getInputEchoState :: IO EchoState
getInputEchoState = if minTTY
                       then fmap MinTTY getInputEchoSTTY
                       else fmap DefaultTTY $ hGetEcho stdin

-- RGS TODO
getInputEchoSTTY :: IO STTYSettings
getInputEchoSTTY = sttyRaw "-g"

-- RGS TODO
setInputEcho :: Bool -> IO ()
setInputEcho echo = if minTTY
                       then setInputEchoSTTY $ ['-' | not echo] ++ "echo"
                       else hSetEcho stdin echo

-- RGS TODO
setInputEchoState :: EchoState -> IO ()
setInputEchoState (MinTTY settings) = setInputEchoSTTY settings
setInputEchoState (DefaultTTY echo) = hSetEcho stdin echo

-- RGS TODO
setInputEchoSTTY :: STTYSettings -> IO ()
setInputEchoSTTY = void . sttyRaw

-- RGS TODO
bracketInputEcho :: IO a -> IO a
bracketInputEcho action = bracket getInputEcho setInputEcho (const action)

-- RGS TODO
withoutInputEcho :: IO a -> IO a
withoutInputEcho action = bracketInputEcho (setInputEchoState echoOff >> action)

-- RGS TODO
sttyRaw :: String -> IO STTYSettings
sttyRaw arg = do
  let stty = (shell $ "stty " ++ arg) {
        std_in  = UseHandle stdin
      , std_out = CreatePipe
      }
  (_, mbStdout, _, rStty) <- createProcess stty
  exStty <- waitForProcess rStty
  case exStty of
    e@ExitFailure{} -> throw e
    ExitSuccess     -> maybe (return "") hGetContents mbStdout

-- RGS TODO
data EchoState = MinTTY STTYSettings | DefaultTTY Bool
  deriving (Eq, Ord, Show)

-- RGS TODO
echoOff :: EchoState
echoOff = if minTTY then MinTTY "-echo" else DefaultTTY False

-- RGS TODO
echoOn :: EchoState
echoOn = if minTTY then MinTTY "echo" else DefaultTTY True

-- RGS TODO
type STTYSettings = String

minTTY :: Bool
#if defined(WINDOWS)
minTTY = unsafePerformIO $ do
  h <- getStdHandle sTD_INPUT_HANDLE
  isMinTTYHandle h
{-# NOINLINE minTTY #-}
#else
minTTY = False
#endif
