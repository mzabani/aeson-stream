module MemoryUsageSpec where

import AesonStream (ToJSONStream(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import           Streaming.Prelude              ( Of
                                                , Stream
                                                )
import qualified Streaming.Prelude             as S
import Test.Hspec (Spec, it, shouldSatisfy)


data VeryLargeObj = VeryLargeObj {
    field1 :: Stream (Of Text) IO (),
    field2 :: Stream (Of Text) IO ()
    }
    deriving stock Generic
    deriving anyclass ToJSONStream

spec :: Spec
spec = do
    it "Stream more than fits in memory" $ do
        -- This uses more memory than my machine or free CI gives us, so should be
        -- sufficient to test.
        let numChars = 1024 * 1024 * 1000
            largeArray = S.take numChars $ S.repeat "A"
        l <- S.fold_ (\acc _el -> acc + 1) (0 :: Integer) id $ toJSONStream $ VeryLargeObj largeArray largeArray 
        l `shouldSatisfy` (> (fromIntegral numChars * 2))