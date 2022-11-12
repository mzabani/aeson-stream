This is just a toy Haskell library to encode JSON objects in streaming fashion. It provides a `ToJSONStream` class that can be _anyclass_ derived for types which have a `Generic` instance.

It also provides a `UseToJSONInstance` class for those with existing bespoke `ToJSON` instances for their types.
Suppose you have very intricate `ToJSON` instance for a type and you want it to derive a `ToJSONStream` instance for it without writing it by hand again, then you can just:


```haskell
deriving via (UseToJSONInstance MyType) instance (ToJSONStream MyType)
```

Naturally, this will use the `ToJSON` instance underneath so it won't stream encode your type when serializing.

### How streaming is it?

- Objects' fields are each streamed separately (and recursively)
- Arrays' elements are each streamed separately (and recursively)
- `Text`, `String` are streamed whole, not incrementally.
- `Bool` and numeric types are streamed whole, not incrementally.