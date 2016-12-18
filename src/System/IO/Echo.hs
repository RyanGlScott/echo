{-# LANGUAGE CPP #-}

{-|
Module:      System.IO.Echo
Copyright:   (C) 2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: Portable

Exports functions that handle whether or not terminal input is handled in a way
that should be portable across different platforms and consoles.
-}
module System.IO.Echo (
      -- RGS TODO
      getEchoState, getEchoSTTY
    , setEchoState, setEchoSTTY
    , withoutEcho, promptWithoutEcho
    , EchoState, STTYSettings
    ) where

import Control.Exception (bracket)
import System.IO (Handle)

-- RGS TODO
getEchoState :: Handle -> IO EchoState
getEchoState = undefined

getEchoSTTY :: Handle -> IO STTYSettings
getEchoSTTY = undefined

-- RGS TODO
setEchoState :: Handle -> EchoState -> IO ()
setEchoState = undefined

setEchoSTTY :: Handle -> STTYSettings -> IO ()
setEchoSTTY = undefined

withoutEcho :: Handle -> IO a -> IO a
withoutEcho h action = bracket (getEchoState h) (setEchoState h) (const action)

promptWithoutEcho :: IO a -> IO a
promptWithoutEcho = undefined

-- RGS TODO
type EchoState = Either STTYSettings Bool

-- RGS TODO
type STTYSettings = String

#if defined(WINDOWS)

#endif
