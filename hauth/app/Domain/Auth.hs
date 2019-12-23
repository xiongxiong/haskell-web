module Domain.Auth (
    -- * Types
    Auth(..),
    Email,
    mkEmail,
    rawEmail,
    Password,
    mkPassword,
    rawPassword,
    UserId,
    VerificationCode,
    SessionId,
    RegistrationErr(..),
    EmailValidationErr(..),
    EmailVerificationErr(..),
    LoginErr(..),

    -- * Ports
    AuthRepo(..),
    EmailVerificationNotif(..),
    SessionRepo(..),

    -- * Use cases
    register
    , verifyEmail
    , login
    , resolveSessionId
    , getUser
    ) where

import ClassyPrelude
import Domain.Validation
import Text.Regex.PCRE.Heavy
import Control.Monad.Except
import Katip

data Auth = Auth
    {
        authEmail :: Email,
        authPassword :: Password
    } deriving (Show, Eq)

data RegistrationErr = 
    RegistrationErrEmailTaken deriving (Show, Eq)

newtype Email = Email {emailRaw :: Text} deriving (Show, Eq, Ord)

rawEmail :: Email -> Text
rawEmail = emailRaw
mkEmail :: Text -> Either [Text] Email
mkEmail = validate Email [regexMatches [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|] "Not a valid email"]
data EmailValidationErr = EmailValidationErrInvalidEmail

newtype Password = Password {passwordRaw :: Text} deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
    [
        lengthBetween 5 50 "Should between 5 and 50",
        regexMatches [re|\d|] "Should contain number",
        regexMatches [re|[A-Z]|] "Should contain uppercase letter",
        regexMatches [re|[a-z]|] "Should contain lowercase letter"
    ]

data PasswordValidationErr 
    = PasswordValidationErrLength Int
    | PasswordValidationErrMustContainUpperCase
    | PasswordValidationErrMustContainLowerCase
    | PasswordValidationErrMustContainNumber

type VerificationCode = Text

data EmailVerificationErr = EmailVerificationErrInvalidCode deriving (Show, Eq)

class Monad m => AuthRepo m where
    addAuth :: Auth -> m (Either RegistrationErr (UserId, VerificationCode))
    setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationErr (UserId, Email))
    findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
    findEmailFromUserId :: UserId -> m (Maybe Email)

instance AuthRepo IO where
    addAuth (Auth email pass) = do
        putStrLn $ "adding auth: " <> rawEmail email
        return $ Right (0, "fake verification code")
    setEmailAsVerified vcode = do
        putStrLn $ "pretend verification code is correct"
        return $ Right (0, Email "fake email")
    findUserByAuth auth = do
        putStrLn $ "fake user"
        return $ Just (0, True)
    findEmailFromUserId uId = do
        putStrLn $ "fake user email"
        return $ Just (Email {emailRaw = "test@example.com"}) 

class Monad m => EmailVerificationNotif m where
    notifyEmailVerification :: Email -> VerificationCode -> m ()

instance EmailVerificationNotif IO where
    notifyEmailVerification email vcode =
        putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode

class Monad m => SessionRepo m where
    newSession ::  UserId -> m SessionId
    findUserIdBySessionId :: SessionId -> m (Maybe UserId)

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

type UserId = Int

type SessionId = Text

data LoginErr 
    = LoginErrInvalidAuth 
    | LoginErrEmailNotVerified 
    deriving (Show, Eq)

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