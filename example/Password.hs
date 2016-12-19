{-|
Module:      Password
Copyright:   (C) 2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: Portable

A simple program which prompts you for your username and password (without
leaking your password onto the screen as you type it).
-}
module Main (main) where

import System.IO (hFlush, stdout)
import System.IO.Echo (withoutInputEcho)

main :: IO ()
main = do
    putLabel "Username: "
    username <- getLine
    putLabel "Password: "
    password <- withoutInputEcho getLine

    putStrLn ""
    putStrLn "-----------------------------------"
    putStrLn $ "Your username is: " ++ username
    putStrLn $ "Your password is: " ++ password
  where
    putLabel label = putStr label >> hFlush stdout
