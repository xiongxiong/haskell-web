module Adapter.HTTP.API.Auth where

import ClassyPrelude
import Web.Scotty.Trans
import Domain.Auth
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:))
import Adapter.HTTP.Common
import Adapter.HTTP.API.Common
import Network.HTTP.Types.Status
import Data.Aeson hiding (json, (.:))
import Katip

type Routes e m = (ScottyError e, MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m)

routes :: Routes e m => ScottyT e m ()
routes = do
    -- register
    post "/api/auth/register" actionRegister
    -- verify email
    post "/api/auth/verifyEmail" actionVerifyEmail
    -- login
    post "/api/auth/login" actionLogin
    -- get user
    get "/api/users" actionUsers

actionRegister :: Routes e m => ActionT e m ()
actionRegister = do
    input <- parseAndValidateJSON authForm
    domainResult <- lift $ register input
    case domainResult of
        Left RegistrationErrEmailTaken -> do
            status status400
            json ("EmailTaken" :: Text)
        _ -> return ()

authForm :: (Monad m) => DF.Form [Text] m Auth
authForm = Auth <$> "email" .: emailForm <*> "password" .: passwordForm
    where
        emailForm = DF.validate (toResult . mkEmail) (DF.text Nothing)
        passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)

actionVerifyEmail :: Routes e m => ActionT e m ()
actionVerifyEmail = do
    input <- parseAndValidateJSON verifyEmailForm
    domainResult <- lift $ verifyEmail input
    case domainResult of
        Left EmailVerificationErrInvalidCode -> do
            status status400
            json ("InvalidCode" :: Text)
        _ -> return ()

verifyEmailForm :: (Monad m) => DF.Form [Text] m VerificationCode
verifyEmailForm = DF.text Nothing

actionLogin :: Routes e m => ActionT e m ()
actionLogin = do
    input <- parseAndValidateJSON authForm
    domainResult <- lift $ login input
    case domainResult of
        Left LoginErrInvalidAuth -> do
            status status400
            json ("InvalidAuth" :: Text)
        Left LoginErrEmailNotVerified -> do
            status status400
            json ("EmailNotVerified" :: Text)
        Right sId -> do
            setSessionIdInCookie sId
            return ()

actionUsers :: Routes e m => ActionT e m ()
actionUsers = do
    userId <- reqCurrentUserId
    mayEmail <- lift $ getUser userId
    case mayEmail of
        Nothing -> raise $ stringError "Should not happen: SessionId map to invalid UserId"
        Just email -> json $ rawEmail email