{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{-|
Module:      System.IO.Echo
Copyright:   (C) 2016-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: Portable

Exports functions that handle whether or not terminal input is handled in a way
that should be portable across different platforms and consoles.
-}
module System.IO.Echo (
      -- * Public interface
      withoutInputEcho, bracketInputEcho
    , getInputEchoState, setInputEchoState
    , EchoState, echoOff, echoOn

      -- * Alternative interface
    , getInputEcho, setInputEcho
    ) where

import System.IO.Echo.Internal
