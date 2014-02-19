{-# LANGUAGE TemplateHaskell #-}

module Handler.Echo where

import Import

getEchoR :: Text -> Handler Html
getEchoR text = defaultLayout $(widgetFile "echo")
