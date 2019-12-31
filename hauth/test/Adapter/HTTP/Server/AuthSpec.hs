module Adapter.HTTP.Server.AuthSpec where

import ClassyPrelude
import Test.Hspec
import Adapter.HTTP.Fixture
import Test.Hspec.Wai.JSON
import Test.Hspec.Wai.Matcher
import Test.Hspec.Wai
import Domain.Auth.Types

spec :: Spec
spec = do
    describe "POST /api/auth/register" $ do
        let emailTakenFixture = emptyFixture
                {
                    _register = \_ -> return $ Left RegistrationErrEmailTaken
                }
        with (app emailTakenFixture) $
            it "should reject account creation" $
                post "/api/auth/register" [json|{"email": "abc@test.com", "password": "abcDEF123"}|]
                    `shouldRespondWith` [json|"EmailTaken"|] {matchStatus = 400}