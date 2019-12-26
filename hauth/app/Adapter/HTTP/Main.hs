module Adapter.HTTP.Main where

import Domain.Auth 
import ClassyPrelude
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Adapter.HTTP.API.Auth as AuthAPI
import Adapter.HTTP.Common
import Katip
import Network.Wai
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger

type Main m = (MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m)

main :: Main m => Int -> (m Response -> IO Response) -> IO ()
main port runner = scottyT port runner routes

routes :: Main m => ScottyT LText m ()
routes = do
    middleware $ gzip $ def { gzipFiles = GzipCompress }
    middleware logStdout
    AuthAPI.routes
    defaultHandler $ \e -> do
        lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
        status status500
        json ("InternalServerError" :: Text)