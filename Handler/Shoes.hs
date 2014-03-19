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

import Settings.PhotoPath( shoePhotoPath )
import Control.Exception.Lifted hiding (Handler)

data ShoeReq = ShoeReq {
        _reqDesc  :: Text,
        _reqColor :: Text,  -- Sum type?
        _reqSize  :: Text,  -- Validation?
        reqPhoto  :: Text   -- String is represented as Text in aeson anyway.
    }

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
--  XXX: Photo is saved to a file; the filename is specified by
--       the Id. But the Id is not stored in the database, so one
--       cannot store photo path in the DB neither.
postShoesR :: Handler ()
postShoesR = do
    shoeReq <- parseJsonBody_  -- will respond with "invalid args" on error
    let mphoto = decodeBase64 $ reqPhoto shoeReq
    maybe invalidContent (saveShoe shoeReq) mphoto
    -- redirect $ ShoeR shoeId  -- will the service be used by humans?

  where saveShoe shoeReq photo = do
          shoeId <- runDB . insert $ shoeFromReq shoeReq
          saveFile (shoePhotoPath shoeId) photo
           `onException` runDB (delete shoeId) -- clean up orphan record.

        decodeBase64 = hush . Base64.decode . Text.encodeUtf8
        saveFile name photo = liftIO $ BS.writeFile name photo
        invalidContent = invalidArgs ["invalid base64-encoded image"]

getShoesR :: Handler Html
getShoesR = do
    shoes <- runDB $ selectList [] []
    defaultLayout $(widgetFile "shoes")
