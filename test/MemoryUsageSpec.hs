module MemoryUsageSpec where

import AesonStream (ToJSONStream(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import           Streaming.Prelude              ( Of
                                                , Stream
                                                )
import qualified Streaming.Prelude             as S
import Test.Hspec (Spec, it, shouldSatisfy)


data VeryLargeObj m = VeryLargeObj {
    field1 :: Stream (Of Text) m (),
    field2 :: Stream (Of Text) m ()
    }
    deriving stock Generic
    deriving anyclass (ToJSONStream m)

spec :: Spec
spec = do
    it "Stream more than fits in memory" $ do
        -- 2^30 =~ 1GB chars 'A' streamed, plus a few characters that form
        -- a JSON object `{ "field1": "AAA..", "field2:" "AAA..." }`
        -- means more than `numChars` characters streamed in total.
        -- This test only works if we limit GHC's runtime memory usage somehow, though.
        -- I'm pretty sure it might not be testing memory memory usage correctly right now.
        let numChars :: Int = 1024 * 1024 * 1024
            largeArray = S.take numChars $ S.repeat "A"
        l <- S.fold_ (\acc _el -> acc + 1) (0 :: Integer) id $ toJSONStream $ VeryLargeObj @IO largeArray largeArray 
        l `shouldSatisfy` (> (fromIntegral numChars * 2))