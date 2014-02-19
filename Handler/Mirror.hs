{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Mirror where

import Import
import qualified Data.Text as Text

getMirrorR :: Handler Html
getMirrorR = defaultLayout $(widgetFile "mirror")

postMirrorR :: Handler Html
postMirrorR = do
        postedText <- runInputPost $ ireq textField "content"
        let reversed = Text.reverse postedText
        defaultLayout $(widgetFile "posted")
