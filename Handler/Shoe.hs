{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Shoe
    ( getShoeR
    , deleteShoeR
    , postDeleteShoeR
    )
where

import Import
import Settings.PhotoPath( shoePhotoPath )
import System.FilePath( (</>) )

import System.Directory
import Control.Exception hiding (Handler)
import System.IO.Error

getShoeR :: ShoeId -> Handler Html
getShoeR shoeId = do
    shoe <- runDB $ get404 shoeId
    defaultLayout $ do
        setTitle . toHtml $ shoeDescription shoe
        -- XXX: Need to explicitly prepend ".." to photo path,
        --      otherwise it tries to go to "shoe/....". Can it
        --      be easily fixed?
        $(widgetFile "shoe")

deleteShoeR :: ShoeId -> Handler ()
deleteShoeR shoeId = do
    runDB $ delete shoeId
    liftIO . removeIfExists $ shoePhotoPath shoeId

    setMessage "Successfully deleted"
    redirect ShoesR

-- XXX: Replace form submit with JavaScript and you'll never need
--      to use POST instead of DELETE
postDeleteShoeR :: ShoeId -> Handler ()
postDeleteShoeR = deleteShoeR

removeIfExists :: FilePath -> IO ()
removeIfExists fname = removeFile fname `catch` handleExists
  where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e
