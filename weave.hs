#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

-- to compile use $ stack ghc -- -O2 -threaded weave.hs
{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = sh $ do
    filename <- find (suffix ".hs") "src"
    let newname = dropExtension filename <.> "md"
    printf (fp%" -> "%fp%"\n") filename newname
    inshell (format ("./weave.sed "%fp) filename ) empty & output newname
    filename <- find (suffix ".hs") "test"
    let newname = dropExtension filename <.> "md"
    printf (fp%" -> "%fp%"\n") filename newname
    inshell (format ("./weave.sed "%fp) filename ) empty & output newname

