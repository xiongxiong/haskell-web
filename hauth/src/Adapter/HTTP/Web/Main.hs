module Adapter.HTTP.Web.Main where

import Domain.Auth.Types
import ClassyPrelude
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Katip
import qualified Adapter.HTTP.Web.Auth as Auth
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Network.Wai

type Env m = (MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m, AuthService m)

main :: Env m => (m Response -> IO Response) -> IO Application
main runner = do
    cacheContainer <- initCaching PublicStaticCaching
    scottyAppT runner $ routes cacheContainer

routes :: Env m => CacheContainer -> ScottyT LText m ()
routes cachingStrategy = do
    middleware $ gzip $ def {gzipFiles = GzipCompress}
    middleware $ staticPolicy' cachingStrategy (addBase "src/Adapter/HTTP/Web")
    middleware $ logStdout
    Auth.routes
    notFound $ text "Not found"
    defaultHandler $ \e -> do
        lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
        text "Internal server error!"