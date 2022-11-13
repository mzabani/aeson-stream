module AesonValueGen where

import           Data.Aeson                     ( Value(..) )
import qualified Data.HashMap.Strict           as HM
import           Data.Scientific                ( Scientific
                                                , fromFloatDigits
                                                )
import qualified Data.Vector                   as V
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

aesonValueGen :: forall m . MonadGen m => m Value
aesonValueGen = Gen.recursive
    Gen.choice
    [
    -- Non-recursive generators
      pure Null
    , String <$> Gen.text (Range.linear 0 200) Gen.unicode
    , Number <$> Gen.choice
        [ fromIntegral <$> Gen.integral
            (Range.linearFrom @Integer 0 (-99999999999) 99999999999)
        , fromFloatDigits
            <$> Gen.double (Range.linearFracFrom 0 (-99999999999) 99999999999)
        ]
    , Bool <$> Gen.bool
    ]
    [
    -- Recursive generators
      Array <$> (V.fromList <$> valueListGen 0 10 aesonValueGen)
    , Object
        <$> (HM.fromList <$> valueListGen
                0
                10
                (   (,)
                <$> Gen.text (Range.linear 0 30) Gen.unicode
                <*> aesonValueGen
                )
            )
    ]
  where
    valueListGen minSize maxSize elementGen =
        Gen.list (Range.linear minSize maxSize) elementGen
