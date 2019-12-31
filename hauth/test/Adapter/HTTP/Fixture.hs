module Adapter.HTTP.Fixture where

import ClassyPrelude
import Domain.Auth.Types
import Katip
import Network.Wai
import qualified Adapter.HTTP.Main as HTTP
import Fixture

data Fixture m = Fixture
    {
          _register :: Auth -> m (Either RegistrationErr ())
        , _verifyEmail :: VerificationCode -> m (Either EmailVerificationErr ())
        , _login :: Auth -> m (Either LoginErr SessionId)
        , _resolveSessionId :: SessionId -> m (Maybe UserId)
        , _getUser :: UserId -> m (Maybe Email)
        , _addAuth :: Auth -> m (Either RegistrationErr (UserId, VerificationCode))
        , _setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationErr (UserId, Email))
        , _findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
        , _findEmailFromUserId :: UserId -> m (Maybe Email)
        , _notifyEmailVerification :: Email -> VerificationCode -> m ()
        , _newSession :: UserId -> m SessionId
        , _findUserIdBySessionId :: SessionId -> m (Maybe UserId)
    }

emptyFixture :: Fixture IO
emptyFixture = Fixture
    {
          _register = const unimplemented
        , _verifyEmail = const unimplemented
        , _login = const unimplemented
        , _resolveSessionId = const unimplemented
        , _getUser = const unimplemented
        , _addAuth = const unimplemented
        , _setEmailAsVerified = const unimplemented
        , _findUserByAuth = const unimplemented
        , _findEmailFromUserId = const unimplemented
        , _notifyEmailVerification = \_ _ -> unimplemented
        , _newSession = const unimplemented
        , _findUserIdBySessionId = const unimplemented
    }

newtype App a = App
    {
        unApp :: ReaderT (Fixture IO) (KatipContextT IO) a
    } deriving (Applicative, Functor, Monad, MonadReader (Fixture IO), MonadIO, KatipContext, Katip)

app :: Fixture IO -> IO Application
app fixture = do
    le <- initLogEnv "HAuth" "test"
    let runner = runKatipContextT le () mempty . flip runReaderT fixture . unApp
    HTTP.app runner

instance AuthService App where
    register = dispatch _register
    verifyEmail = dispatch _verifyEmail
    login = dispatch _login
    resolveSessionId = dispatch _resolveSessionId
    getUser = dispatch _getUser

instance AuthRepo App where
    addAuth = dispatch _addAuth
    setEmailAsVerified = dispatch _setEmailAsVerified
    findUserByAuth = dispatch _findUserByAuth
    findEmailFromUserId = dispatch _findEmailFromUserId

instance EmailVerificationNotif App where
    notifyEmailVerification = dispatch2 _notifyEmailVerification

instance SessionRepo App where
    newSession = dispatch _newSession
    findUserIdBySessionId = dispatch _findUserIdBySessionId
