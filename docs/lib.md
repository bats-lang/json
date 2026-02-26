# lib

### `datavtype json =
  | json_null of ()
  | json_bool of (bool)
  | json_int of (int)
  | json_str of ([l:agz] $A.arr(byte, l, 4096), int)
  | json_arr of (json_list)
  | json_obj of (json_entries)`

### `fun json_free(v: json): void`

### `fun json_list_free(lst: json_list): void`

### `fun json_entries_free(ents: json_entries): void`

### `fun serialize(v: !json, b: !$B.builder): void`

### `fun serialize_list(lst: !json_list, b: !$B.builder, first: bool): void`

### `fun serialize_entries(ents: !json_entries, b: !$B.builder, first: bool): void`

### `fun parse {l:agz}{n:pos}
  (src: !$A.borrow(byte, l, n), pos: int, max: int n
  ): $R.result(@(json, int), int)`
