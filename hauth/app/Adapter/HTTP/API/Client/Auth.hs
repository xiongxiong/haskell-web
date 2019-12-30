module Adapter.HTTP.API.Client.Auth where

import ClassyPrelude
import Network.HTTP.Client
import Data.Has
import qualified Domain.Auth as D
import Network.HTTP.Types
import Adapter.HTTP.API.Types.Auth ()
import Adapter.HTTP.API.Client.Common
import Data.Aeson
import Control.Monad.Catch

register :: HttpClient r m => D.Auth -> m (Either D.RegistrationErr ())
register auth = do
    State initReq mgr <- asks getter
    let req = initReq
            {
                  method = "POST"
                , path = "/api/auth/register"
                , requestBody = RequestBodyLBS $ encode auth
            }
    resp <- liftIO $ httpLbs req mgr
    case responseStatus resp of
        (Status 200 _) -> return $ Right ()
        _ -> Left <$> parseOrErr req resp

verifyEmail :: HttpClient r m => D.VerificationCode -> m (Either D.EmailVerificationErr ())
verifyEmail vCode = do
    State initReq mgr <- asks getter
    let req = initReq
            {
                  method = "POST"
                , path = "/api/auth/verifyEmail"
                , requestBody = RequestBodyLBS $ encode vCode
            }
    resp <- liftIO $ httpLbs req mgr
    case responseStatus resp of
        (Status 200 _) -> return $ Right ()
        _ -> Left <$> parseOrErr req resp        

login :: HttpClient r m => D.Auth -> m (Either D.LoginErr Session)
login auth = do
    State initReq mgr <- asks getter
    let req = initReq
            {
                  method = "POST"
                , path = "/api/auth/login"
                , requestBody = RequestBodyLBS $ encode auth
            }
    resp <- liftIO $ httpLbs req mgr
    case responseStatus resp of
        (Status 200 _) -> return $ Right $ responseCookieJar resp
        _ -> Left <$> parseOrErr req resp

getUser :: HttpClient r m => Session -> m D.Email
getUser session = do
    State initReq mgr <- asks getter
    let req = initReq
            {
                  method = "GET"
                , path = "/api/users"
                , cookieJar = Just session
            }
    resp <- liftIO $ httpLbs req mgr
    case responseStatus resp of
        (Status 200 _) -> parseOrErr req resp
        _ -> throwM $ UnexpectedResponse req resp