module MemoryUsageSpec where

import AesonStream (ToJSONStream(..), UseToJSONInstance(..))
import Control.DeepSeq (force, NFData)
import Data.Aeson (ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import GHC.Generics (Generic)
import           Streaming.Prelude              ( Of
                                                , Stream
                                                )
import qualified Streaming.Prelude             as S
import Test.Hspec (Spec, it, shouldSatisfy, shouldBe, xit)
import Data.ByteString (ByteString)
import Control.Exception (onException, evaluate, SomeException (SomeException), try)
import Data.Maybe (mapMaybe, catMaybes)
import Control.Exception.Base (catch)
import Debug.Trace
import Control.Concurrent (newMVar, modifyMVar_, readMVar)
import Control.Monad (void)


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
    deriving stock (Generic)
    deriving anyclass (NFData, ToJSON, ToJSONStream m)

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
