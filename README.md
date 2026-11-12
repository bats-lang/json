# json

JSON serialization and deserialization for the [Bats](https://github.com/bats-lang) programming language.

`parse` reads any JSON text (RFC 8259, ECMA-404) exactly, within two
deliberate limits: `STRING_CAP` (1048576) bytes for a string (value or
key) once decoded and for a number lexeme, and `DEPTH_CAP` (512) levels
of nested arrays and objects. Past them, and on anything JSON does not
allow, it fails with a `parse_error` that says what failed and where.

  A lone surrogate (`\ud800` not followed by a low one, or a lone
  `\udc00`) becomes U+FFFD, as Go's `encoding/json` and the WHATWG
  `TextEncoder` do (see the comment on `str_loop` in `src/lib.bats` for
  the alternatives). So every string `parse` returns is well-formed
  UTF-8.
## API

`bats check` generates the API reference in `docs/lib.md`.

## Safety

Safe library — `unsafe = false`. No `$UNSAFE`, no `$extfcall`. Every
array access is proven in bounds by the types, including the exact
allocation of each string: it is decoded into 4096-byte chunks whose
total is in the type, then copied into an array of exactly that size.
