{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 702
# if defined(WINDOWS)
{-# LANGUAGE Trustworthy #-}
# else
#  if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#  else
{-# LANGUAGE Trustworthy #-}
#  endif
# endif
#endif

{-|
Module:      System.IO.Echo.Internal
Copyright:   (C) 2016-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: Portable

Exports functions that handle whether or not terminal input is handled in a way
that should be portable across different platforms and consoles.

Unlike "System.IO.Echo", this module exports internal functionality which, if
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

# if MIN_VERSION_Win32(2,5,0)
import System.Win32.MinTTY (isMinTTYHandle)
# else
import System.IO.Echo.MinTTY (isMinTTYHandle)
# endif
import System.IO.Unsafe (unsafePerformIO)
#endif

-- | Return whether the terminal's echoing is on ('True') or off ('False').
--
-- Note that while this works on MinTTY, it is not as efficient as
-- 'getInputEchoState', as it involves a somewhat expensive substring
-- computation.
getInputEcho :: IO Bool
getInputEcho = if minTTY
                  then do settings <- sttyRaw "-a"
                          -- This assumes that other settings come after
                          -- [-]echo in the output of `stty -a`. Luckily, this
                          -- seems to be the case on every incarnation of
                          -- MinTTY that I've tried.
                          return $ not ("-echo " `isInfixOf` settings)
                  else hGetEcho stdin

-- | Return the terminal's current input 'EchoState'.
getInputEchoState :: IO EchoState
getInputEchoState = if minTTY
                       then fmap MinTTY getInputEchoSTTY
                       else fmap DefaultTTY $ hGetEcho stdin

-- | Return all of @stty@'s current settings in a non-human-readable format.
--
-- This function is not very useful on its own. Its greater purpose is to
-- provide a compact 'STTYSettings' that can be fed back into
-- 'setInputEchoState'.
getInputEchoSTTY :: IO STTYSettings
getInputEchoSTTY = sttyRaw "-g"

-- | Set the terminal's echoing on ('True') or off ('False').
setInputEcho :: Bool -> IO ()
setInputEcho echo = if minTTY
                       then setInputEchoSTTY $ ['-' | not echo] ++ "echo"
                       else hSetEcho stdin echo

-- | Set the terminal's input 'EchoState'.
setInputEchoState :: EchoState -> IO ()
setInputEchoState (MinTTY settings) = setInputEchoSTTY settings
setInputEchoState (DefaultTTY echo) = hSetEcho stdin echo

-- | Create an @stty@ process and wait for it to complete. This is useful for
-- changing @stty@'s settings, after which @stty@ does not output anything.
--
-- @
-- setInputEchoSTTY = 'void' . 'sttyRaw'
-- @
setInputEchoSTTY :: STTYSettings -> IO ()
setInputEchoSTTY = void . sttyRaw

-- | Save the terminal's current input 'EchoState', perform a computation,
-- restore the saved 'EchoState', and then return the result of the
-- computation.
--
-- @
-- bracketInputEcho action = 'bracket' 'getInputEchoState' 'setInputEchoState' (const action)
-- @
bracketInputEcho :: IO a -> IO a
bracketInputEcho action = bracket getInputEchoState setInputEchoState (const action)

-- | Perform a computation with the terminal's input echoing disabled. Before
-- running the computation, the terminal's input 'EchoState' is saved, and the
-- saved 'EchoState' is restored after the computation finishes.
--
-- @
-- withoutInputEcho action = 'bracketInputEcho' ('setInputEchoState' 'echoOff' >> action)
-- @
withoutInputEcho :: IO a -> IO a
withoutInputEcho action = bracketInputEcho (setInputEchoState echoOff >> action)

-- | Create an @stty@ process, wait for it to complete, and return its output.
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

-- | A representation of the terminal input's current echoing state. Example
-- values include 'echoOff' and 'echoOn'.
data EchoState
  = MinTTY STTYSettings
    -- ^ The argument to (or value returned from) an invocation of the @stty@
    -- command-line utility. Most POSIX-like shells have @stty@, including
    -- MinTTY on Windows. Since neither 'hGetEcho' nor 'hSetEcho' work on
    -- MinTTY, when 'getInputEchoState' runs on MinTTY, it returns a value
    -- built with this constructor.
    --
    -- However, native Windows consoles like @cmd.exe@ or PowerShell do not
    -- have @stty@, so if you construct an 'EchoState' with this constructor
    -- manually, take care not to use it with a native Windows console.
  | DefaultTTY Bool
    -- ^ A simple on ('True') or off ('False') toggle. This is returned by
    -- 'hGetEcho' and given as an argument to 'hSetEcho', which work on most
    -- consoles, with the notable exception of MinTTY on Windows. If you
    -- construct an 'EchoState' with this constructor manually, take care not
    -- to use it with MinTTY.
  deriving (Eq, Ord, Show)

-- | Indicates that the terminal's input echoing is (or should be) off.
echoOff :: EchoState
echoOff = if minTTY then MinTTY "-echo" else DefaultTTY False

-- | Indicates that the terminal's input echoing is (or should be) on.
echoOn :: EchoState
echoOn = if minTTY then MinTTY "echo" else DefaultTTY True

-- | Settings used to configure the @stty@ command-line utility.
type STTYSettings = String

-- | Is the current process attached to a MinTTY console (e.g., Cygwin or MSYS)?
minTTY :: Bool
#if defined(WINDOWS)
minTTY = unsafePerformIO $ do
  h <- getStdHandle sTD_INPUT_HANDLE
  isMinTTYHandle h
{-# NOINLINE minTTY #-}
#else
minTTY = False
#endif
