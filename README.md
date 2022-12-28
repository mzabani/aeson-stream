This is just a toy Haskell library to encode large JSON objects in streaming fashion. It provides a `ToJSONStream` class that can be _anyclass_ derived for types which have a `Generic` instance. As in the example below:

```haskell
data VeryLargeObj m = VeryLargeObj {
    field1 :: Stream (Of Text) m (),
    field2 :: Stream (Of Text) m ()
    }
    deriving stock Generic
    deriving anyclass (ToJSONStream m)
```

This library also provides a `UseToJSONInstance` newtype wrapper for those with existing bespoke `ToJSON` instances for their types.
So if you have a `ToJSON` instance for a type and you want to derive a `ToJSONStream` instance for it without duplicating the code, you can just:


```haskell
deriving via (UseToJSONInstance MyType) instance (Monad m => ToJSONStream m MyType)
```

Naturally, this will use the `ToJSON` instance underneath so it won't stream your type incrementally when serializing.

### How streaming is it?

- Objects' fields are each streamed separately (and recursively, and reasonably lazily)
- Arrays' elements are each streamed separately (and recursively, and reasonably lazily)
- `Text` and `String` are streamed whole, not incrementally.
- `Bool` and numeric types are streamed whole, not incrementally.