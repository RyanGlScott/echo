# `echo`
[![Hackage](https://img.shields.io/hackage/v/echo.svg)][Hackage: echo]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/echo.svg)](http://packdeps.haskellers.com/reverse/echo)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Linux build](https://github.com/RyanGlScott/echo/workflows/Haskell-CI/badge.svg)](https://github.com/RyanGlScott/echo/actions?query=workflow%3AHaskell-CI)
[![Windows build](https://ci.appveyor.com/api/projects/status/a0dh9v7j995tjj2u?svg=true)](https://ci.appveyor.com/project/RyanGlScott/echo)

[Hackage: echo]:
  http://hackage.haskell.org/package/echo
  "echo package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

The `base` library exposes the `hGetEcho` and `hSetEcho` functions for querying and setting echo status, but unfortunately, neither function works with MinTTY consoles on Windows. This is a serious issue, since `hGetEcho` and `hSetEcho` are often used to disable input echoing when a program prompts for a password, so many programs will reveal your password as you type it on MinTTY!

This library provides an alternative interface which works with both MinTTY and other consoles. An example is included which demonstrates how one might prompt for a password using this library. To build it, make sure to configure with the `-fexample` flag.
