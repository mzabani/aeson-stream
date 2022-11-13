module MemoryUsageSpec where

import AesonStream (ToJSONStream(..), UseToJSONInstance(..))
import AesonValueGen (aesonValueGen)
import Control.DeepSeq (force, NFData)
import Control.Exception (onException, evaluate, SomeException (SomeException), try, catch)
import Control.Concurrent (newMVar, modifyMVar_, readMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), FromJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (mapMaybe, catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import           Streaming.Prelude              ( Of
                                                , Stream
                                                )
import qualified Streaming.Prelude             as S
import Test.Hspec (Spec, it, shouldSatisfy, shouldBe, xit)
import  Test.Hspec.Hedgehog (hedgehog, forAll)


data VeryLargeObj m = VeryLargeObj {
    field1 :: Stream (Of Text) m (),
    field2 :: Stream (Of Text) m ()
    }
    deriving stock Generic
    deriving anyclass (ToJSONStream m)

data CustomJsonInstance = CustomJsonInstance {
    c1 :: Maybe Bool,
    c2 :: Int,
    c3 :: [Int]
}

instance ToJSON CustomJsonInstance where
    toJSON CustomJsonInstance{..} = toJSON (c1, c2, c3)

deriving via (UseToJSONInstance CustomJsonInstance) instance (Monad m => ToJSONStream m CustomJsonInstance)

data SillyObject = SillyObject {
    s1 :: Bool,
    s2 :: Int,
    s3 :: [Int]
}
    deriving stock (Eq, Generic, Show)
    deriving anyclass (NFData, FromJSON, ToJSON, ToJSONStream m)

spec :: Spec
spec = do
    xit "Stream more than fits in memory" $ do
        -- 2^30 =~ 1GB chars 'A' streamed, plus a few characters that form
        -- a JSON object `{ "field1": "AAA..", "field2:" "AAA..." }`
        -- means more than `numChars` characters streamed in total.
        -- This test only works if we limit GHC's runtime memory usage somehow, though.
        -- I'm pretty sure it might not be testing memory memory usage correctly right now.
        let numChars :: Int = 1024 * 1024 * 1024
            largeArray = S.take numChars $ S.repeat "A"
        l <- S.fold_ (\acc _el -> acc + 1) (0 :: Integer) id $ toJSONStream $ VeryLargeObj @IO largeArray largeArray
        l `shouldSatisfy` (> (fromIntegral numChars * 2))

    it "UseToJSONInstance properly generates ToJSONStream instances" $ do
        let i = CustomJsonInstance (Just False) 37 [1, 2, 3, 4]
        streamedValue <- S.fold_ @IO (<>) "" id $ toJSONStream i
        Aeson.encode i `shouldBe` streamedValue

    it "Lazily evaluate objects fields" $ do
        let i = SillyObject False 37 (error "Error when evaluating the last field")
        evaluatedChunks <- newMVar @LBS.ByteString ""
        void $ try @SomeException $ S.foldM_ @IO (\() bs -> do
                bss <- evaluate $ force bs
                modifyMVar_ evaluatedChunks (pure . (<> bss))
                ) (pure ()) pure $ toJSONStream i
        
        evaluatedParts <- readMVar evaluatedChunks
        fullValue <- S.fold_ @IO (<>) "" id $ toJSONStream i { s3 = [] }
        -- Notice we just need to close the array and the object with the partially evaluated bits
        evaluatedParts <> "]}" `shouldBe` fullValue

    it "Lazily evaluate objects in lists" $ do
        let i = [1 :: Int, 2, 3, 4, error "Error when evaluating the last value" ]
        evaluatedChunks <- newMVar @LBS.ByteString ""
        void $ try @SomeException $ S.foldM_ @IO (\() bs -> do
                bss <- evaluate $ force bs
                modifyMVar_ evaluatedChunks (pure . (<> bss))
                ) (pure ()) pure $ toJSONStream i
        
        evaluatedParts <- readMVar evaluatedChunks
        fullValue <- S.fold_ @IO (<>) "" id $ toJSONStream [1 :: Int, 2, 3, 4, 5]

        -- The comma and the last element are missing. The comma could perhaps be yielded
        -- in a valid and lazier implementation, but this is lazy enough and if this behaviour
        -- ever changes we can just change this test.
        evaluatedParts <> ",5]" `shouldBe` fullValue

    xit "Tuples?" $ putStrLn "TODO"

    it "With arbitrarily generated values, Aeson.decode . toJSONStream is the identity" $ hedgehog $ do
        jsonVal <- forAll aesonValueGen
        encodedVal <- S.fold_ (<>) "" id $ toJSONStream jsonVal
        -- liftIO $ print encodedVal
        liftIO $ Aeson.decode encodedVal `shouldBe` Just jsonVal

    it "With Generic generated instances, Aeson.decode . toJSONStream is the identity" $ hedgehog $ do
        -- We only test one type's Generic instance here. This is probably not sufficient.
        let i = SillyObject False 37 [1,2]
        encodedVal <- S.fold_ (<>) "" id $ toJSONStream i
        -- liftIO $ print encodedVal
        liftIO $ Aeson.decode encodedVal `shouldBe` Just i