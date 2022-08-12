{-# LANGUAGE TemplateHaskell #-}

module Sections.Deciphering where

import Arc.Widgets.Code
import Data.FileEmbed
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Reflex.Dom
import Slideshow

deciphering :: (DomBuilder t m, PostBuild t m) => Slideshow t m ()
deciphering = do
    section "Deciphering the Moon Language"
    slide $ do
        setSlideTitle "Language extensions"
        setSlideContent $
            unorderedList $ do
                li' $ code "Haskell" "{-# LANGUAGE OverloadedStrings #-}"
                li $ text "Haskell is a living language"
                li $ text "Extensions introduce new idioms"
                li $ text "Everyone has preferences"
                li $ text "Some extensions are safer than others"
    slide $ do
        setSlideTitle "Basic syntax"
        setSlideContent $ do
            unorderedList $ do
                li $ text "Function application and operator precedence is confusing"
                li $ code' "Haskell" "main = mapM_ print take 10 fibs 0 0"
                li $ code' "Haskell" "main = mapM_ print (take 10 (fibs 0 0))"
                li $ code' "Haskell" "main = mapM_ print $ take 10 $ fibs 0 0"
    slide $ do
        setSlideTitle "Expressions and their types"
        setSlideContent $ do
            unorderedList $ do
                li $ text "Type signatures are for humans, not computers"
                li $ text "Use " >> el "code" (text "TypeApplications") >> text " to help the compiler"
                li $ do
                    code' "Haskell" "read \"5\" :: Int"
                    code' "Haskell" "read @Int \"5\""
                    code' "Cpp" "read<int>(\"5\")"
    slide $ do
        setSlideTitle "Typeclasses"
        setSlideContent $ do
            unorderedList $ do
                li $ text "Most languages have interfaces"
                li $ code' "Haskell" $ Text.decodeUtf8 $(embedFile "app/code/typeclasses.hs")
    slide $ do
        setSlideTitle "The mighty Functor"
        setSlideContent $ do
            code "Haskell" "f <$> x"
            unorderedList $ do
                li $ text "Muscle of the Haskell ecosystem"
                li $ text "Function application in a context"
                li $ text "Removes most boilerplate"
                li $ text "Requires a combination of intuition and knowledge"
                liClass "manicule" $ text "Absolutely critical before progressing"
    slide $ do
        setSlideClassName "slide quote-slide"
        setSlideContent $
            appendContent $
                el "figure" $
                    el "blockquote" $ do
                        el "p" $ text "There is a Law of Reversed Effort. The harder we try with the conscious will to do something, the less we shall succeed."
                        el "p" $ text "We cannot make ourselves understand; the most we can do is to foster a state of mind, in which understanding may come to us."
                        el "figcaption" $ do
                            text "Aldous Huxley, "
                            el "cite" $ text "Complete Essays"
    slide $ do
        setSlideTitle "Monads"
        setSlideContent $ do
            unorderedList $ do
                li $ text "Just functors with some added flair"
                li $ text "You already understand monads"
    slide $ do
        setSlideClassName "slide quote-slide"
        setSlideContent $
            appendContent $
                el "figure" $
                    el "blockquote" $ do
                        el "p" $ text "It's not that I'm so smart, it's just that I stay with problems longer."
                        el "figcaption" $ text "Albert Einstein"
