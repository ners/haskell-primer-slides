{-# LANGUAGE TemplateHaskell #-}

module Sections.HelloHaskell where

import Arc.Widgets.Code
import Data.FileEmbed
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Reflex.Dom
import Slideshow

helloHaskell :: (DomBuilder t m, PostBuild t m) => Slideshow t m ()
helloHaskell = do
    section "Hello, Haskell"
    slide $ do
        setSlideTitle "Many ways to play with Haskell"
        setSlideContent $
            unorderedList $ do
                li $ do
                    text "Run Haskell in your browser: "
                    elAttr "a" ("href" =: "https://replit.com/languages/haskell") $
                        text "https://replit.com/languages/haskell"
                li $ text "Play with Haskell in a REPL with GHCi"
                li $ text "Run Haskell programs with runhaskell"
                li $ text "Compile Haskell programs with GHC"
                li $ text "Develop Haskell projects with Cabal"
                li $ text "... Stack?"
    slide $ do
        setSlideTitle "Getting the tools"
        setSlideContent $
            unorderedList $ do
                li' $ do
                    timedEl "span" $ text "From official repos"
                    timedEl "span" $ text " ... on Ubuntu?"
                    timedEl "span" $ text " ... on Arch?"
                li $ text "Haskell Platform"
                li $ text "GHCup"
                li $ text "... Stack?"
    slide $ do
        setSlideTitle "Lowering the barrier to entry"
        setSlideContent $ do
            code "Bash" "nix shell nixpkgs#ghc"
            unorderedList $ do
                li $ text "Does not hijack your project or OS"
                li $ text "Works the same on any distro"
                li $ text "Always up to date, but versions can also be specified"
                li $ text "Easy to start with, scales linearly"
                liClass "manicule" $ text "Nix extends beyond Haskell!"
    slide $ do
        setSlideTitle "Hello C with Nix"
        setSlideContent $ do
            code "Nix" $ Text.decodeUtf8 $(embedFile "app/code/hello-c.nix")
    slide $ do
        setSlideTitle "Hello Haskell with Nix"
        setSlideContent $ do
            code "Nix" $ Text.decodeUtf8 $(embedFile "app/code/hello-haskell.nix")
    slide $ do
        setSlideContent $ do
            code "Nix" $ Text.decodeUtf8 $(embedFile "app/code/hello-haskell-full.nix")
