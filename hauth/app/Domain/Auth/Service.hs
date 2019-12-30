module Domain.Auth.Service where

import ClassyPrelude
import Domain.Auth.Types
import Control.Monad.Except
import Katip

withUserIdContext :: (KatipContext m) => UserId -> m a -> m a
withUserIdContext uId = katipAddContext (sl "userId" uId)

register :: (KatipContext m, AuthRepo m, EmailVerificationNotif m)
    => Auth -> m (Either RegistrationErr ())
register auth = runExceptT $ do
    (uId, vCode) <- ExceptT $ addAuth auth
    let email = authEmail auth
    lift $ notifyEmailVerification email vCode
    withUserIdContext uId $
        $(logTM) InfoS $ ls (rawEmail email) <> " is registered successfully"

verifyEmail :: (KatipContext m, AuthRepo m) => VerificationCode -> m (Either EmailVerificationErr ())
verifyEmail vCode = runExceptT $ do
    (uId, email) <- ExceptT $ setEmailAsVerified vCode
    withUserIdContext uId $
        $(logTM) InfoS $ ls (rawEmail email) <> " is verified successfully"
    return ()

login :: (KatipContext m, AuthRepo m, SessionRepo m) => Auth -> m (Either LoginErr SessionId)
login auth = runExceptT $ do
    result <- lift $ findUserByAuth auth
    case result of
        Nothing -> throwError LoginErrInvalidAuth
        Just (_, False) -> throwError LoginErrEmailNotVerified
        Just (uId, _) -> withUserIdContext uId . lift $ do
            sId <- newSession uId
            $(logTM) InfoS $ ls (rawEmail $ authEmail auth) <> " logged in successfully"
            return sId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId
