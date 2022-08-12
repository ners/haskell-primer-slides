{-# LANGUAGE TemplateHaskell #-}

module Sections.Conclusion where

import qualified Data.ByteString as BS
import Data.ByteString.Base64
import Data.FileEmbed
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Reflex.Dom
import Slideshow

conclusion :: (DomBuilder t m, PostBuild t m) => Slideshow t m ()
conclusion = do
    slide $ do
        setSlideClassName "slide title-slide"
        setSlideContent $ appendContent $ text "Thank you for coming to my TED talk."
    slide $ do
        setSlideClassName "slide image-slide"
        setSlideContent $
            appendContent $ do
                let src = "data:image/png;base64," <> encodeBase64 $(embedFile "app/images/stammtisch.png")
                elAttr "img" ("src" =: src) blank
