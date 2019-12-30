module Domain.Auth.ServiceSpec where

import ClassyPrelude
import Domain.Auth.Types
import Katip

data Fixture m = Fixture
    {
          _addAuth :: Auth -> m (Either RegistrationErr (UserId, VerificationCode))
        , _setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationErr (UserId, Email))
        , _findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
        , _findEmailFromUserId :: UserId -> m (Maybe Email)
        , _notifyEmailVerification :: Email -> VerificationCode -> m ()
        , _newSession :: UserId -> m SessionId
        , _findUserIdBySessionId :: SessionId -> m (Maybe UserId)
    }

unimplemented :: a
unimplemented = error "unimplemented"

emptyFixture :: Fixture a
emptyFixture = Fixture
    {
          _addAuth = const unimplemented
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

dispatch :: (MonadIO m, MonadReader r m) => (r -> a -> IO b) -> (a -> m b)
dispatch getter param = do
    func <- asks getter
    liftIO $ func param

dispatch2 :: (MonadIO m, MonadReader r m) => (r -> a -> b -> IO c) -> (a -> b -> m c)
dispatch2 getter param1 param2 = do
    func <- asks getter
    liftIO $ func param1 param2

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

runApp :: Fixture IO -> App a -> IO a
runApp fixture action = do
    le <- initLogEnv "HAuth" "test"
    runKatipContextT le () mempty . flip runReaderT fixture . unApp $ action