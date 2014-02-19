module Settings.PhotoPath
    ( fullName
    )
where

import Import
import System.FilePath( (</>), (<.>) )

-- TODO: That needs to be configurable. Move to configs.
-- TODO: Create directories at startup (if necessary).
photoPath :: FilePath
photoPath = "static" </> "img" </> "shoes"

photoName :: ShoeId -> FilePath
photoName shoeId = "shoe" <> extractKey (unKey shoeId)
    where extractKey (PersistInt64 k) = show k
          extractKey _ = error "MIGRATION TO MONGO SHALL NOT BE PAINLESS :)\n"
                            ++ "extractKey expects PersistInt64"

fullName :: ShoeId -> FilePath
fullName shoeId = photoPath </> photoName shoeId <.> "jpg"
