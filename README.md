# json

JSON serialization and deserialization for the [Bats](https://github.com/bats-lang) programming language.

## JSON Value Type

```
json = json_null
     | json_bool(bool)
     | json_int(int)
     | json_str(arr(byte, 4096), int)
     | json_arr(json_list)
     | json_obj(json_entries)
```

Arrays are linked lists of JSON values. Objects are association lists of (key, value) pairs.

## Usage

### Serialize

```bats
#use json as J
#use builder as B

val v = $J.json_obj(
  $J.json_entries_cons(key_arr, key_len,
    $J.json_int(42),
    $J.json_entries_nil()))
val b = $B.create()
val () = $J.serialize(v, b)
val () = $J.json_free(v)
```

### Deserialize

```bats
val r = $J.parse(input_bv, 0, input_max)
case+ r of
| ~$R.ok(@(v, end_pos)) => let
    (* use v *)
    val () = $J.json_free(v)
  in end
| ~$R.err(pos) => println! ("parse error at ", pos)
```

### Roundtrip

```bats
(* parse → serialize → compare *)
val r = $J.parse(input_bv, 0, max)
case+ r of
| ~$R.ok(@(v, _)) => let
    val b = $B.create()
    val () = $J.serialize(v, b)
    val () = $J.json_free(v)
    (* b contains the re-serialized JSON *)
  in end
```

## API

See [docs/lib.md](docs/lib.md) for the full API reference.

## Safety

Safe library — `unsafe = false`. No `$UNSAFE`, no `$extfcall`.

## Tests

Unit tests verify roundtrips for: null, true/false, integers, strings, arrays, objects, and nested structures.
