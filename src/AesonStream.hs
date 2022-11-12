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

instance (Monad m, ToJSONStream m a, KnownSymbol fieldName) => JG m (S1 ('MetaSel ('Just fieldName) x y z) (K1 i a)) where
    jg (M1 v) =
        S.yield "\""
            >> S.yield (encodeUtf8 $ LT.pack $ symbolVal (Proxy @fieldName))
            >> S.yield "\":"
            >> toJSONStream (unK1 v)

instance (Monad m, JG m a) => JG m (D1 x (C1 ('MetaCons y z 'True) a)) where
    jg (M1 (M1 fields)) = S.yield "{" >> jg fields >> S.yield "}"

-- instance (ToJSONStream m a, ToJSONStream b) => JG (a :*: b) where
--     jg (a :*: b) = \x y -> toJSONStream x >> S.yield "," >> toJSONStream y

newtype UseToJSONInstance a = UseToJSONInstance a

instance (Monad m, ToJSON a) => ToJSONStream m (UseToJSONInstance a) where
    toJSONStream (UseToJSONInstance v) = S.yield $ encode v

deriving via (UseToJSONInstance Int) instance (Monad m => ToJSONStream m Int)
deriving via (UseToJSONInstance Float) instance (Monad m => ToJSONStream m Float)
deriving via (UseToJSONInstance Double) instance (Monad m => ToJSONStream m Double)
deriving via (UseToJSONInstance Bool) instance (Monad m => ToJSONStream m Bool)
deriving via (UseToJSONInstance LT.Text) instance (Monad m => ToJSONStream m LT.Text)
deriving via (UseToJSONInstance T.Text) instance (Monad m => ToJSONStream m T.Text)

-- | This instance emits JSON arrays from a stream of jsonifiable values.
instance (Monad m, ToJSONStream m a) => ToJSONStream m (Stream (Of a) m ()) where
    toJSONStream v =
        S.yield "[" >> S.intersperse "," (S.for v toJSONStream) >> S.yield "]"

instance (Monad m, ToJSONStream m a) => ToJSONStream m [a] where
    -- Does this lazily traverse the list?
    -- TODO: Take advantage of the other instance?
    toJSONStream v =
        S.yield "["
            >> S.intersperse "," (S.for (S.each v) toJSONStream)
            >> S.yield "]"

instance (Monad m, ToJSONStream m a) => ToJSONStream m (Maybe a) where
    toJSONStream Nothing  = S.yield "null"
    toJSONStream (Just v) = toJSONStream v

data TestObject m = TestObject
    { age           :: Int
    , streamArray   :: Stream (Of Int) m ()
    , field         :: [Int]
    , thirdField    :: Maybe Bool
    , lazyTextField :: LT.Text
    , otherObj      :: Maybe (TestObject m)
    }
    deriving stock Generic
    deriving anyclass (ToJSONStream m)