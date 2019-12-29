module Adapter.HTTP.API.Server.Main where

import Domain.Auth 
import ClassyPrelude
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Adapter.HTTP.API.Auth as Auth
import Adapter.HTTP.API.Server.Common
import Katip
import Network.Wai
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger

type Main m = (MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m)

main :: Main m => (m Response -> IO Response) -> IO Application
main runner = scottyAppT runner routes

routes :: Main m => ScottyT LText m ()
routes = do
    middleware $ gzip $ def { gzipFiles = GzipCompress }
    middleware logStdout
    Auth.routes
    notFound $ do
        json $ errorResponse ("NotFound" :: Text)
    defaultHandler $ \e -> do
        lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
        json $ errorResponse ("InternalServerError" :: Text)