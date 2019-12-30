module Adapter.RabbitMQ.Auth where

import ClassyPrelude
import Adapter.RabbitMQ.Common
import Network.AMQP
import Katip
import Data.Aeson
import Data.Aeson.TH
import qualified Domain.Auth.Types as D
import Control.Monad.Catch

data EmailVerificationPayload = EmailVerificationPayload
    {
          emailVerificationPayloadEmail :: Text
        , emailVerificationPayloadVerificationCode :: Text
    } deriving (Show)

$(
    let structName = fromMaybe "" . lastMay . splitElem '.' . show $ ''EmailVerificationPayload
        lowercaseFirst (x:xs) = toLower [x] <> xs
        lowercaseFirst xs = xs
        options = defaultOptions
                    {
                        fieldLabelModifier = lowercaseFirst . drop (length structName)
                    }
    in deriveJSON options ''EmailVerificationPayload
 )

notifyEmailVerification :: (Rabbit r m) => D.Email -> D.VerificationCode -> m ()
notifyEmailVerification email vCode = 
    let payload = EmailVerificationPayload (D.rawEmail email) vCode
    in  publish "auth" "userRegistered" payload

consumeEmailVerification :: (EmailVerificationSender m, KatipContext m, MonadCatch m)
    => (m Bool -> IO Bool) -> Message -> IO Bool
consumeEmailVerification runner msg =
    runner $ consumeAndProcess msg handler
    where
        handler payload = do 
            case D.mkEmail (emailVerificationPayloadEmail payload) of
                Left err -> withMsgAndErr msg err $ do
                    $(logTM) ErrorS "Email format is invalid. Rejecting."
                    return False
                Right email -> do
                    let vCode = emailVerificationPayloadVerificationCode payload
                    sendEmailVerification email vCode
                    return True

init :: (EmailVerificationSender m, KatipContext m, MonadCatch m)
    => State -> (m Bool -> IO Bool) -> IO ()
init state runner = do
    initQueue state "verifyEmail" "auth" "userRegistered"
    initConsumer state "verifyEmail" (consumeEmailVerification runner)

class (Monad m) => EmailVerificationSender m where
    sendEmailVerification :: D.Email -> D.VerificationCode -> m ()