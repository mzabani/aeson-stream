{-# LANGUAGE AllowAmbiguousTypes, TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module AesonStream
    ( ToJSONStream(..)
    , UseToJSONInstance(..)
    , genericToJSONStream
    ) where

import           Data.Aeson                     ( ToJSON
                                                , Value(..)
                                                , encode
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.HashMap.Strict           as HM
import           Data.Proxy                     ( Proxy(..) )
import           Data.Ratio                     ( Ratio )
import           Data.Scientific                ( Scientific )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import           Data.Text.Lazy.Encoding       as LT
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           GHC.Generics
import           Streaming.Prelude              ( Of
                                                , Stream
                                                )
import qualified Streaming.Prelude             as S

import           Control.Monad                  ( when )
import           GHC.TypeLits                   ( KnownSymbol
                                                , symbolVal
                                                )


class ToJSONStream m a where
    toJSONStream :: a -> Stream (Of ByteString) m ()
    default toJSONStream :: (Monad m, Generic a, JG m (Rep a)) => a -> Stream (Of ByteString) m ()
    toJSONStream = genericToJSONStream

genericToJSONStream
    :: (Monad m, Generic a, JG m (Rep a)) => a -> Stream (Of ByteString) m ()
genericToJSONStream v = jg (from v)

class JG m f where
    jg :: f x -> Stream (Of ByteString) m ()

instance ToJSONStream m a => JG m (K1 i a) where
    jg (K1 v) = toJSONStream v

instance (Monad m, JG m a, JG m b) => JG m (a :*: b) where
    jg (a :*: b) = jg a >> S.yield "," >> jg b

encodeFieldAndValue
    :: (Monad m, ToJSONStream m a)
    => LT.Text
    -> a
    -> Stream (Of ByteString) m ()
encodeFieldAndValue fieldName v =
    S.yield "\""
        >> S.yield (LT.encodeUtf8 fieldName)
        >> S.yield "\":"
        >> toJSONStream v

instance (Monad m, ToJSONStream m a, KnownSymbol fieldName) => JG m (S1 ('MetaSel ('Just fieldName) x y z) (K1 i a)) where
    jg (M1 v) =
        encodeFieldAndValue (LT.pack $ symbolVal (Proxy @fieldName)) (unK1 v)

instance (Monad m, JG m a) => JG m (D1 x (C1 ('MetaCons y z 'True) a)) where
    jg (M1 (M1 fields)) = S.yield "{" >> jg fields >> S.yield "}"

-- | Derive via this newtype to create an instance that uses `ToJSON` underneath.
-- This can be useful for types with custom `ToJSON` instances.
newtype UseToJSONInstance a = UseToJSONInstance a

instance (Monad m, ToJSON a) => ToJSONStream m (UseToJSONInstance a) where
    toJSONStream (UseToJSONInstance v) = S.yield $ encode v

deriving via (UseToJSONInstance Int) instance (Monad m => ToJSONStream m Int)
deriving via (UseToJSONInstance Integer) instance (Monad m => ToJSONStream m Integer)
deriving via (UseToJSONInstance Float) instance (Monad m => ToJSONStream m Float)
deriving via (UseToJSONInstance Double) instance (Monad m => ToJSONStream m Double)
deriving via (UseToJSONInstance (Ratio a)) instance ((Integral a, Monad m, ToJSON a) => ToJSONStream m (Ratio a))
deriving via (UseToJSONInstance Scientific) instance (Monad m => ToJSONStream m Scientific)
deriving via (UseToJSONInstance Bool) instance (Monad m => ToJSONStream m Bool)
deriving via (UseToJSONInstance LT.Text) instance (Monad m => ToJSONStream m LT.Text)
deriving via (UseToJSONInstance T.Text) instance (Monad m => ToJSONStream m T.Text)

-- | This instance emits JSON arrays from a stream of jsonifiable values.
instance (Monad m, ToJSONStream m a) => ToJSONStream m (Stream (Of a) m ()) where
    toJSONStream v = do
        S.yield "["
        let ms :: Stream (Of (Stream (Of ByteString) m (), Integer)) m () =
                S.zip (S.map (toJSONStream @m) v) (S.each [(1 :: Integer) ..])
        -- There is no S.for_ as hlint suggests :(
        S.for ms (\(vs, i) -> when (i > 1) (S.yield ",") >> vs)
        S.yield "]"

instance (Monad m, ToJSONStream m a) => ToJSONStream m [a] where
    toJSONStream v = toJSONStream $ S.each @m v

instance (Monad m, ToJSONStream m a) => ToJSONStream m (Vector a) where
    toJSONStream v = toJSONStream $ V.toList v

instance (Monad m, ToJSONStream m a) => ToJSONStream m (Maybe a) where
    toJSONStream Nothing  = S.yield "null"
    toJSONStream (Just v) = toJSONStream v

-- | An instance for aeson's `Value`
instance Monad m => ToJSONStream m Value where
    toJSONStream = \case
        Null     -> S.yield "null"
        String s -> toJSONStream s
        Bool   b -> toJSONStream b
        Number n -> toJSONStream n
        Array  v -> toJSONStream v
        Object o -> do
            S.yield "{"
            let ms :: [(Stream (Of ByteString) m (), Integer)]
                ms = zip
                    ( map
                            (\(field, v) ->
                                encodeFieldAndValue (LT.fromStrict field) v
                            )
                    $ HM.toList o
                    )
                    [1 ..]
            -- There is no S.for_ as hlint suggests :(
            S.for (S.each ms) (\(vs, i) -> when (i > 1) (S.yield ",") >> vs)
            S.yield "}"

