module Main where

import Arc.Main
import Slides
import Style

main :: IO ()
main = arcMainWithCss mainStyle slides
