# json

JSON serialization and deserialization for the [Bats](https://github.com/bats-lang) programming language.

`parse` reads any JSON text (RFC 8259, ECMA-404) exactly, within two
deliberate limits: `STRING_CAP` (1048576) bytes for a string (value or
key) once decoded and for a number lexeme, and `DEPTH_CAP` (512) levels
of nested arrays and objects. Past them, and on anything JSON does not
allow, it fails with a `parse_error` that says what failed and where.

## JSON Value Type

```
json = json_null
     | json_bool(bool)
     | json_num(arr(byte, c), int len, option(int))
     | json_str(arr(byte, c), int dlen)
     | json_arr(json_list)
     | json_obj(json_entries)
```

The int index of `json(sz)` bounds the bytes `serialize` writes for the
value: 4 for `null`, 5 for a bool, `len + 4` for a number, `6 * dlen + 2`
for a string, the parts plus 2 for an array or object.

* **`json_str(arr, dlen)`** is the string decoded: the first `dlen`
  bytes of `arr`, which holds at least `dlen` (`parse` allocates exactly
  `dlen`, or 1 byte for the empty string). Every escape is decoded, a
  surrogate pair (`\ud83d\ude00`) to one code point, written as UTF-8.
  A lone surrogate (`\ud800` not followed by a low one, or a lone
  `\udc00`) becomes U+FFFD, as Go's `encoding/json` and the WHATWG
  `TextEncoder` do (see the comment on `str_loop` in `src/lib.bats` for
  the alternatives). So every string `parse` returns is well-formed
  UTF-8.
* **`json_num(lexeme, len, i)`** is a number as its lexeme, exactly as
  the text had it (the first `len` bytes), and `$R.some(i)` when the
  lexeme is an integer (no fraction, no exponent) that fits in a 32-bit
  int, else `$R.none()` (`1.0`, `1e2`, `2147483648`). `-0` has the int 0.
  `json_num_of_int(i)` builds one from an int.
* Arrays are linked lists of JSON values. Objects are association lists
  of (key, value) pairs, keys decoded like strings, in the text's order,
  duplicates kept.

## Usage

### Deserialize

```bats
#use json as J
#use result as R

(* the whole of src[0, n) is one JSON text *)
case+ $J.parse_text(src, n) of
| ~$R.ok(v) => let
    (* use v *)
    val () = $J.json_free(v)
  in end
| ~$R.err(e) => (case+ e of
  | ~$J.StringTooLong(p) => println! ("a string starting at ", p, " is over 1 MiB")
  | ~$J.TooDeep(p) => println! ("nested too deep at ", p)
  | e => println! ("not JSON at ", $J.parse_error_pos(e)))
```

`parse(src, pos, max)` reads the one value at `pos` (after whitespace)
and returns it with the offset just past it, ignoring what follows;
`parse_text(src, max)` reads `src[0, max)` as one JSON text and fails
with `TrailingData` if anything but whitespace follows the value.

### Errors

`parse_error` is a datavtype; each constructor carries a byte offset in
the input. Match it with `case+` (or free it with `parse_error_pos`,
which returns the offset).

| Constructor | What failed | Offset |
|---|---|---|
| `UnexpectedEnd` | the input ended inside a value | the end |
| `UnexpectedByte` | a byte that cannot come here (`+1`, `.5`, `NaN`, `[1 2]`, `[1,]`, `{a:1}`) | the byte |
| `BadNumber` | a number the grammar does not allow: leading zero (`01`), lone `-`, `1.`, `1e`, `-Infinity` | the number's start |
| `NumberTooLong` | a number lexeme over `STRING_CAP` bytes | the number's start |
| `StringTooLong` | a string over `STRING_CAP` bytes decoded | its opening quote |
| `ControlInString` | a raw byte below 0x20 in a string | the byte |
| `BadEscape` | `\` followed by anything but `"\/bfnrtu` | the backslash |
| `BadHex` | `\u` not followed by four hex digits | the backslash |
| `InvalidUtf8` | a byte in a string that does not begin well-formed UTF-8 | the byte |
| `TooDeep` | an array or object nested past `DEPTH_CAP` | its bracket |
| `TrailingData` | `parse_text` only: more than whitespace after the value | the first such byte |

### Serialize

```bats
#use json as J
#use builder as B

val v = $J.json_obj(
  $J.json_entries_cons(key_arr, key_len,
    $J.json_num_of_int(42),
    $J.json_entries_nil()))
val b = $B.create()
val () = $J.serialize(v, b)
val () = $J.json_free(v)
```

`serialize` writes to a builder, proven by the size index to fit
(`BUILDER_CAP`, 512 KiB). `serialize_rope(v, r)` writes the same text
to a `$B.rope`, for a value of any size, such as one `parse` returned
(its size index is not known statically) or a string near the cap.

Both write valid JSON for every value: in strings they escape `"` and
`\`, write `\b \f \n \r \t` for those bytes and `\u00XX` for the other
bytes below 0x20, keep well-formed UTF-8 as it is, and write `\ufffd`
for each byte that does not begin well-formed UTF-8 (a string built by
hand may hold any bytes). A number is written as its lexeme, or as
`null` when the lexeme is not a JSON number (as `JSON.stringify` writes
`null` for `NaN`), which `parse` never produces.

### Roundtrip

```bats
case+ $J.parse_text(src, n) of
| ~$R.ok(v) => let
    val r = $B.rope_create()
    val () = $J.serialize_rope(v, r)
    val () = $J.json_free(v)
    val cs = $B.rope_chunks(r) (* the JSON text, in chunks *)
    ...
  in end
| ~$R.err(e) => ...
```

## API

`bats check` generates the API reference in `docs/lib.md`.

## Safety

Safe library — `unsafe = false`. No `$UNSAFE`, no `$extfcall`. Every
array access is proven in bounds by the types, including the exact
allocation of each string: it is decoded into 4096-byte chunks whose
total is in the type, then copied into an array of exactly that size.

## Tests

* Unit tests (`bats test`): serialization of each kind of value, and
  parse round trips.
* `tests/dynamic/grammar`: every escape, surrogate pairs and lone
  surrogates, raw UTF-8 (valid and not), control bytes, every number
  form the grammar allows and the ones it does not, structure (commas,
  colons, trailing data), each `parse_error` with its offset, and
  serialization (escaping, `\ufffd`, `null` for a bad lexeme).
* `tests/dynamic/caps`: strings and keys past 4096 bytes, and strings
  just under, at and just over the 1 MiB cap (also when the last
  escape, raw UTF-8 sequence or surrogate pair crosses it); number
  lexemes at and over it; nesting at and over `DEPTH_CAP`; round trips
  through `serialize_rope` at the cap.
* `tests/dynamic/limits` and `tests/dynamic/parse`: large arrays, int
  bounds, truncated inputs.

All dynamic tests run under valgrind, which must find no leak.
