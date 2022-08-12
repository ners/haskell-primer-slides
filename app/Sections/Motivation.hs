{-# LANGUAGE TemplateHaskell #-}

module Sections.Motivation where

import Arc.Widgets.Code
import qualified Data.ByteString as BS
import Data.ByteString.Base64
import Data.FileEmbed
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Reflex.Dom
import Slideshow

motivation :: (DomBuilder t m, PostBuild t m) => Slideshow t m ()
motivation = do
    section "Motivation"
    slide $ do
        setSlideTitle "Who is Haskell for?"
        setSlideContent $ do
            timedEl "p" $ text "The spectrum of abstraction:"
            unorderedListClass "spectrum" $ do
                pause
                li $ do
                    let src = "data:image/png;base64," <> encodeBase64 $(embedFile "app/images/derp.png")
                    elAttr "img" ("src" =: src <> "class" =: "icon") blank
                    el "div" $ text "data scientists"
                li $ do
                    let src = "data:image/png;base64," <> encodeBase64 $(embedFile "app/images/all-the-things.png")
                    elAttr "img" ("src" =: src <> "class" =: "icon") blank
                    el "div" $ text "software engineers"
                unpause
                unpause
                unpause
                li $ do
                    let src = "data:image/png;base64," <> encodeBase64 $(embedFile "app/images/hmmm.png")
                    elAttr "img" ("src" =: src <> "class" =: "icon") blank
                    el "div" $ text "language researchers"
                pause
                pause
    slide $ do
        setSlideTitle "« But I already know Python... »"
        setSlideSubtitle "... and other jokes you can tell yourself"
        setSlideContent $ do
            unorderedList $ do
                li $ text "How to motivate the initial investment?"
                li $ text "Haskell offers a unique blend of powerful features"
                li' $
                    unorderedList $ do
                        li $ text "strictness"
                        li $ text "purity"
                        li $ text "laziness"
                liClass "manicule" $ text "Show the headache before offering aspirin!"
    slide $ do
        setSlideTitle "Strictness"
        setSlideContent $ do
            unorderedList $ do
                li $ text "Programming is the art of composing abstractions"
                li $ code' "Haskell" "makeSandwich :: 🍞 -> 🥪"
                li $ text "Type checking is a mechanism for verifying the validity of composition"
                li $ code' "Haskell" "makeSandwich 🍎  -- is this valid?"
                li $ code' "Haskell" "eat (makeSandwich 🍞)  -- what does `eat` take?"
    slide $ do
        setSlideTitle "Strictness: the headache"
        setSlideContent $ do
            unorderedList $ do
                li $ text "What would Python do?"
                unpause
                li $
                    code' "Python" $
                        Text.unlines
                            [ "x = 5"
                            , "y = True"
                            , "x + y"
                            ]
                li $ code' "Python" "6"
    slide $ do
        setSlideTitle "Strictness: the headache"
        setSlideContent $ do
            unorderedList $ do
                unpause
                li $ text "What would Python do?"
                unpause
                li $
                    code' "Python" $ Text.decodeUtf8 $(embedFile "app/code/nonsense.py")
                li $ code' "Python" "True"
    slide $ do
        setSlideClassName "slide image-slide"
        setSlideContent $
            appendContent $ do
                let src = "data:image/jpg;base64," <> encodeBase64 $(embedFile "app/images/runtime-compile.jpg")
                elAttr "img" ("src" =: src) blank
    slide $ do
        setSlideContent $ do
            code "Python" $ Text.decodeUtf8 $(embedFile "app/code/parents.py")
            timedElClass "p" "manicule" $ text "Applies to every language with nullable types"
    slide $ do
        setSlideContent $ do
            unorderedList $ do
                li' $ appendContent $ text "What would Java do?"
                li $ code' "Java" $ Text.decodeUtf8 $(embedFile "app/code/parents.java")
    slide $ do
        setSlideTitle "Strictness: the aspirin"
        setSlideContent $ do
            unorderedList $ do
                li $ text "What would Haskell do?"
                li $ code' "Haskell" $ Text.decodeUtf8 $(embedFile "app/code/parents.hs")
    slide $ do
        setSlideTitle "Strictness: the aspirin"
        setSlideContent $ do
            unorderedList $ do
                li' $ appendContent $ text "What would Haskell do?"
                li' $ code "Haskell" $ Text.decodeUtf8 $(embedFile "app/code/parents-notypes.hs")
    slide $ do
        setSlideTitle "Strictness: bonus round"
        setSlideContent $ code "Python" $ Text.decodeUtf8 $(embedFile "app/code/increment.py")
    slide $ do
        setSlideClassName "slide image-slide"
        setSlideContent $
            appendContent $ do
                let src = "data:image/jpg;base64," <> encodeBase64 $(embedFile "app/images/jesus-whip.jpg")
                elAttr "img" ("src" =: src) blank
    slide $ do
        setSlideTitle "Purity"
        setSlideContent $ do
            unorderedList $ do
                li $ text "Given the same input, a function will always have the same output"
                li $ text "And have no side effects"
                li' $ do
                    timedEl "span" $ text "Unless it does IO"
                    timedEl "span" $ text ", in which case it is treated specially"
    slide $ do
        setSlideTitle "Purity: the headache"
        setSlideContent $ do
            unorderedList $ do
                li $ text "Let's play: " >> el "em" (text "Find the Mutation!")
                li $ code' "Javascript" $ Text.decodeUtf8 $(embedFile "app/code/mutation.js")
    slide $ do
        setSlideTitle "Purity: the aspirin"
        setSlideContent $ do
            unorderedList $ do
                li $ text "What would Haskell do?"
                li $ code' "Haskell" $ Text.decodeUtf8 $(embedFile "app/code/mutation.hs")
    slide $ do
        setSlideTitle "Laziness"
        setSlideContent $ do
            unorderedList $ do
                li $ text "Do not compute values until we need them"
                li $ text "Laziness enables more efficient composition:"
                li' $
                    unorderedList $ do
                        li $ text "abstract control flow with reusable combinators"
                        li $ text "model computation with infinite data structures"
                        li $ text "decouple planning from execution"
    slide $ do
        setSlideTitle "Laziness: the headache"
        setSlideContent $ do
            unorderedList $ do
                li $ code' "Haskell" "any :: (a -> Bool) -> [a] -> Bool"
                li $ text "Find any element that passes the given predicate"
                li $ text "Stop when the first " >> el "code" (text "True") >> text " is found"
                li $ text "Correctly handle infinite lists"
    slide $ do
        setSlideTitle "Laziness: the headache"
        setSlideContent $ do
            unorderedList $ do
                li' $
                    code "Haskell" $
                        Text.unlines
                            [ "any :: (a -> Bool) -> [a] -> Bool"
                            , "any p (x:xs) = p x || any p xs"
                            , "any _ [] = False"
                            ]
            unorderedList $ do
                li $ text "We are redefining general recursion here"
                li $ text "Can't we just use " >> el "code" (text "map") >> text " or " >> el "code" (text "filter") >> text "?"
            timedElClass "p" "manicule" $ text "There is no potential for code reuse here."
    slide $ do
        setSlideTitle "Laziness: the aspirin"
        setSlideContent $ do
            unorderedList $ do
                li $
                    code' "Haskell" $
                        Text.unlines
                            [ "any :: (a -> Bool) -> [a] -> Bool"
                            , "any p xs = or (map p xs)"
                            ]
                li $ text "Piggyback the control flow on existing functions"
                li $ text "More direct expression of intent"
            timedElClass "p" "manicule" $ text "This approach scales well to less trivial cases."
    slide $ do
        setSlideClassName "slide quote-slide"
        setSlideContent $
            appendContent $
                el "figure" $
                    el "blockquote" $ do
                        el "p" $ text "A programming language is low level when its programs require attention to the irrelevant."
                        el "figcaption" $ text "Alan J. Perlis"
