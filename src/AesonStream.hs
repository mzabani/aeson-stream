{-# LANGUAGE AllowAmbiguousTypes, TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module AesonStream
    ( ToJSONStream(..)
    , UseToJSONInstance(..)
    , genericToJSONStream
    ) where

import           Data.Aeson                     ( ToJSON
                                                , encode
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )
import           GHC.Generics
import           Streaming.Prelude              ( Of
                                                , Stream
                                                )
import qualified Streaming.Prelude             as S

import           GHC.TypeLits                   ( KnownSymbol
                                                , symbolVal
                                                )


class ToJSONStream a where
    toJSONStream :: a -> Stream (Of ByteString) IO ()
    default toJSONStream :: (Generic a, JG (Rep a)) => a -> Stream (Of ByteString) IO ()
    toJSONStream = genericToJSONStream

genericToJSONStream
    :: (Generic a, JG (Rep a)) => a -> Stream (Of ByteString) IO ()
genericToJSONStream v = jg (from v)

class JG f where
    jg :: f x -> Stream (Of ByteString) IO ()

instance ToJSONStream a => JG (K1 i a) where
    jg (K1 v) = toJSONStream v

instance (JG a, JG b) => JG (a :*: b) where
    jg (a :*: b) = jg a >> S.yield "," >> jg b

instance (ToJSONStream a, KnownSymbol fieldName) => JG (S1 ('MetaSel ('Just fieldName) x y z) (K1 i a)) where
    jg (M1 v) =
        S.yield "\""
            >> S.yield (encodeUtf8 $ LT.pack $ symbolVal (Proxy @fieldName))
            >> S.yield "\":"
            >> toJSONStream (unK1 v)

instance (JG a) => JG (D1 x (C1 ('MetaCons y z 'True) a)) where
    jg (M1 (M1 fields)) = S.yield "{" >> jg fields >> S.yield "}"

-- instance (ToJSONStream a, ToJSONStream b) => JG (a :*: b) where
--     jg (a :*: b) = \x y -> toJSONStream x >> S.yield "," >> toJSONStream y

newtype UseToJSONInstance a = UseToJSONInstance a

instance ToJSON a => ToJSONStream (UseToJSONInstance a) where
    toJSONStream (UseToJSONInstance v) = S.yield $ encode v

deriving via (UseToJSONInstance Int) instance (ToJSONStream Int)
deriving via (UseToJSONInstance Float) instance (ToJSONStream Float)
deriving via (UseToJSONInstance Double) instance (ToJSONStream Double)
deriving via (UseToJSONInstance Bool) instance (ToJSONStream Bool)
deriving via (UseToJSONInstance LT.Text) instance (ToJSONStream LT.Text)
deriving via (UseToJSONInstance T.Text) instance (ToJSONStream T.Text)

-- | This instance emits JSON arrays from a stream of jsonifiable values.
instance ToJSONStream a => ToJSONStream (Stream (Of a) IO ()) where
    toJSONStream v =
        S.yield "[" >> S.intersperse "," (S.for v toJSONStream) >> S.yield "]"

instance ToJSONStream a => ToJSONStream [a] where
    -- Does this lazily traverse the list?
    -- TODO: Take advantage of the other instance?
    toJSONStream v =
        S.yield "["
            >> S.intersperse "," (S.for (S.each v) toJSONStream)
            >> S.yield "]"

instance ToJSONStream a => ToJSONStream (Maybe a) where
    toJSONStream Nothing  = S.yield "null"
    toJSONStream (Just v) = toJSONStream v

data TestObject = TestObject
    { age           :: Int
    , streamArray   :: Stream (Of Int) IO ()
    , field         :: [Int]
    , thirdField    :: Maybe Bool
    , lazyTextField :: LT.Text
    , otherObj      :: Maybe TestObject
    }
    deriving stock Generic
    deriving anyclass ToJSONStream
