{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Shoes
    ( getShoesR
    , postShoesR
    )
where

import Import

import Control.Error( hush )
import Control.Monad( mzero )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as Text

import GHC.Generics( Generic )
import Settings.PhotoPath( fullName )

data ShoeReq = ShoeReq {
        _reqDesc  :: Text,
        _reqColor :: Text,  -- Sum type?
        _reqSize  :: Text,  -- Validation?
        reqPhoto  :: Text   -- String is represented as Text in aeson anyway.
    } deriving (Generic)

instance FromJSON ShoeReq where
    parseJSON (Object v) =
        ShoeReq <$> v .: "description"
                <*> v .: "color"
                <*> v .: "size"
                <*> v .: "photo"

    parseJSON _ = mzero


shoeFromReq :: ShoeReq -> Shoe
shoeFromReq (ShoeReq desc color size _) = Shoe desc color size

-- NOTE: rather unfortunate name: we are POSTing _one_ shoe.
-- TODO: How shall error handling be organized?
-- XXX:  Photo is saved to a file; the filename is specified by
--       the Id. But the Id is not stored in the database, so one
--       cannot store photo path in the DB neither.
-- TODO: If saving to a file fails, the record in a DB need to be
--       deleted. It currently remains orphan.
postShoesR :: Handler ()
postShoesR = do
    shoeReq <- parseJsonBody_ :: Handler ShoeReq  -- will return "invalid args" error
    shoeId <- runDB . insert $ shoeFromReq shoeReq
    let mphoto = decodePhoto shoeReq

    maybe invalidContent (saveFile $ fullName shoeId) mphoto
    -- redirect $ ShoeR shoeId  -- will the service be used by humans?

  where decodePhoto = hush . Base64.decode . Text.encodeUtf8 . reqPhoto
        saveFile name photo = liftIO $ BS.writeFile name photo
        invalidContent = invalidArgs ["invalid base64-encoded image"]

getShoesR :: Handler Html
getShoesR = do
    shoes <- runDB $ selectList [] []
    defaultLayout $(widgetFile "shoes")
