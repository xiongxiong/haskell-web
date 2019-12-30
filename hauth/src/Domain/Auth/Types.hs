module Domain.Auth.Types (
    -- * Types
    Auth(..),
    Email(rawEmail),
    mkEmail,
    Password(rawPassword),
    mkPassword,
    UserId,
    VerificationCode,
    SessionId,
    RegistrationErr(..),
    EmailVerificationErr(..),
    LoginErr(..),

    -- * Services
    AuthRepo(..),
    EmailVerificationNotif(..),
    SessionRepo(..),
    AuthService(..)
    ) where

import ClassyPrelude
import Domain.Validation
import Text.Regex.PCRE.Heavy

newtype Email = Email {rawEmail :: Text} deriving (Show, Eq, Ord)

mkEmail :: Text -> Either [Text] Email
mkEmail = validate Email [regexMatches [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|] "Not a valid email"]

newtype Password = Password {rawPassword :: Text} deriving (Show, Eq)

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
    [
        lengthBetween 5 50 "Should between 5 and 50",
        regexMatches [re|\d|] "Should contain number",
        regexMatches [re|[A-Z]|] "Should contain uppercase letter",
        regexMatches [re|[a-z]|] "Should contain lowercase letter"
    ]

data Auth = Auth
    {
        authEmail :: Email,
        authPassword :: Password
    } deriving (Show, Eq)

type VerificationCode = Text

type UserId = Int

type SessionId = Text

data RegistrationErr = 
    RegistrationErrEmailTaken deriving (Show, Eq)

data EmailVerificationErr = EmailVerificationErrInvalidCode deriving (Show, Eq)

data LoginErr 
    = LoginErrInvalidAuth 
    | LoginErrEmailNotVerified 
    deriving (Show, Eq)

class Monad m => AuthRepo m where
    addAuth :: Auth -> m (Either RegistrationErr (UserId, VerificationCode))
    setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationErr (UserId, Email))
    findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
    findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m => EmailVerificationNotif m where
    notifyEmailVerification :: Email -> VerificationCode -> m ()

class Monad m => SessionRepo m where
    newSession ::  UserId -> m SessionId
    findUserIdBySessionId :: SessionId -> m (Maybe UserId)

class (Monad m) => AuthService m where
    register :: Auth -> m (Either RegistrationErr ())
    verifyEmail :: VerificationCode -> m (Either EmailVerificationErr ())
    login :: Auth -> m (Either LoginErr SessionId)
    resolveSessionId :: SessionId -> m (Maybe UserId)
    getUser :: UserId -> m (Maybe Email)