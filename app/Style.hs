{-# LANGUAGE TemplateHaskell #-}

module Style where

import Arc.Clay.Util
import Clay
import Clay.Stylesheet
import qualified Data.ByteString as BS
import Data.ByteString.Base64
import Data.FileEmbed
import Data.String (IsString (fromString))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Base64 as Text
import Web.Font.MDI (mdiFormatQuoteClose, mdiFormatQuoteOpen)

mainStyle :: Css
mainStyle = do
    html <> body ? do
        margin nil nil nil nil
        padding nil nil nil nil
    main_ ? do
        fontSize (px 64)
        lineHeight (em 1.5)
        fontFamily ["Oswald"] [sansSerif]
        fontWeight $ weight 300
        width (vw 100)
        height (vh 100)
        userSelect none
        overflowX hidden
        overflowY hidden
        ".playing" & cursor cursorNone
    h1 <> h2 <> h3 ? do
        fontWeight $ weight 400
        self |+ ".subtitle" ? do
            opacity 0.5
            fontStyle italic
            fontSize (pct 75)
            marginTop (em (-2))
    time ? do
        position absolute
        right (em 1.5)
        bottom (em 0)
        opacity 0.3
        fontSize (pct 50)
    ".slide" ? do
        width (vw 100)
        height (vh 100)
        overflow auto
        padding2 (em 1) (em 2)
        boxSizing borderBox
        display flex
        flexDirection column
    ".inactive" & do
        display none
    ".title-slide" <> ".section-slide" <> ".image-slide" <> ".quote-slide" ? do
        ".title" ? do
            fontFamily ["Oswald Stencil"] [sansSerif]
            lineHeight (em 1)
        --".subtitle" ? fontStyle italic
        textAlign center
        justifyContent center
    ".image-slide" ? do
        boxSizing borderBox
        img ? do
            key "object-fit" $ Value "contain"
            let src = "data:image/png;base64," <> encodeBase64 $(embedFile "app/images/frame.png")
            key "border-image" $ Value $ "url('" <> Plain src <> "') 93 92 87 92 stretch stretch"
            borderColor "#f4be52"
            borderStyle inset
            borderWidth (px 60)
            display block
            backgroundColor "#ffffee"
            marginAll auto
            flexGrow 1
            width auto
            maxWidth (pct 80)
            maxHeight auto
    ".title-slide" ? ".title" ? fontSize (pct 250)
    ".visible" <> ".invisible" ? do
        transition "opacity" (ms 100) easeInOut (sec 0)
    ".invisible" ? do
        opacity 0
    p <> li ? do
        margin2 (em 0.5) nil
    ul <> pre ? marginAll nil
    ul ? ".spectrum" & do
        display flex
        textAlign center
        margin2 (em 2) nil
        li ? do
            flexGrow 1
            display inlineBlock
            ".icon" ? height (em 5)
    li ? do
        listStyleType none
        ".icon" ? do
            margin2 nil (em 1)
            height (em 1.5)
    ".manicule" ? do
        paddingLeft (em 2.5)
        position relative
        before & do
            content $ stringContent ""
            display inlineBlock
            width (em 2)
            height (em 1)
            position absolute
            left nil
            top (em 0.5)
            backgroundImage $ url $ "data:image/png;base64," <> encodeBase64 $(embedFile "app/images/hand-pointing-right.png")
            backgroundSize contain
            backgroundRepeat noRepeat
    code ? do
        fontFamily ["Monoid"] [monospace]
        fontSize (pct 60)
        lineHeight (em 1.5)
    figure ? do
        position relative
    blockquote ? do
        fontFamily ["Source Serif Variable"] [serif]
        --fontStyle italic
        fontWeight $ weight 400
        after & do
            mdiFont
            content $ charContent mdiFormatQuoteClose
            fontSize (pct 1000)
            opacity 0.1
            position absolute
            bottom nil
            right nil
        textAlign start
    figcaption ? do
        before & content (stringContent "— ")
        fontStyle normal
        fontVariant smallCaps
        fontSize (pct 80)
        marginTop (em 2)
        cite ? do
            fontVariant normal
            fontStyle italic
            fontSize (pct 90)
