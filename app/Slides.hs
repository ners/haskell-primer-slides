{-# LANGUAGE TemplateHaskell #-}

module Slides where

import Control.Monad.Fix
import Data.ByteString.Base64
import Data.FileEmbed
import Reflex.Dom
import Sections.Conclusion
import Sections.Deciphering
import Sections.HelloHaskell
import Sections.Motivation
import Slideshow

slides :: forall t m. (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
slides = slideshow $ do
    slide $ do
        setSlideClassName "slide image-slide"
        setSlideContent $
            appendContent $ do
                let src = "data:image/png;base64," <> encodeBase64 $(embedFile "app/images/kong.png")
                elAttr "img" ("src" =: src) blank
    setSlideshowTitle "A Haskell Primer"
    setSlideshowSubtitle "Or: How I learned to stop worrying and love the Monad"
    titleSlide
    slide $ do
        setSlideClassName "slide quote-slide"
        setSlideContent $
            appendContent $
                el "figure" $
                    el "blockquote" $ do
                        el "p" $ text "There is no royal road to Haskell."
                        el "figcaption" $ text "Euclid"
    motivation
    helloHaskell
    deciphering
    conclusion
