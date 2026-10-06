(* json -- JSON serialization and deserialization *)
(* Safe: no $UNSAFE, no $extfcall *)
(* Size-indexed types: serialize has zero runtime bounds checks. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use builder as B
#use result as R

(* ============================================================
   Caps
   ============================================================ *)

(* The most bytes a string (value or key) holds once decoded, and the
   longest number lexeme: 1 MiB, the most $A.alloc gives. A longer one is
   a parse failure of its own (StringTooLong, NumberTooLong), so no input
   makes parse allocate more than this for one string. *)
#pub stadef STRING_CAP = 1048576

macdef _STRING_CAP = 1048576

(* The deepest nesting of arrays and objects parse reads: the outermost
   array or object is level 1, and one level deeper is TooDeep. Parsing,
   freeing and serializing recurse once per level, so without a limit a
   deep enough text overflows the stack instead of failing: a debug
   native build uses about 800 bytes of stack per level, and a WASM
   build has a 1 MiB stack (bats links it with stack-size=1048576). 512
   levels keep well inside both. Other strict parsers limit it too:
   serde_json to 128, Go's encoding/json to 10000 (Go's stacks grow). *)
#pub stadef DEPTH_CAP = 512

macdef _DEPTH_CAP = 512

(* ============================================================
   JSON value type — size-indexed for compile-time bounds
   The int index is the max serialized bytes.
   ============================================================ *)

(* json_num: a number as its lexeme, exactly as the text had it (the
   first len bytes of the array), and its value as an int when the
   lexeme is an integer (no fraction, no exponent) that fits in 32 bits.
   parse makes only lexemes JSON's number grammar allows; serialize
   writes the lexeme, or null when it is not a JSON number (as
   JSON.stringify writes null for NaN and Infinity).

   json_str: the first dlen bytes of the array, the string decoded:
   escapes decoded, UTF-8. serialize escapes '"', '\\' and every byte
   below 0x20, and writes \ufffd for each byte that does not begin
   well-formed UTF-8, so its output is always valid JSON. *)
#pub datavtype json(int) =
  | json_null(4) of ()
  | json_bool(5) of (bool)
  | {c:pos}{len:pos | len <= c; len <= STRING_CAP}
    json_num(len + 4) of ([l:agz] $A.arr(byte, l, c), int len, $R.option(int))
  | {c:pos}{dlen:nat | dlen <= c; dlen <= STRING_CAP}
    json_str(6 * dlen + 2) of ([l:agz] $A.arr(byte, l, c), int dlen)
  | {sz:nat} json_arr(sz + 2) of (json_list(sz))
  | {sz:nat} json_obj(sz + 2) of (json_entries(sz))

and json_list(int) =
  | json_list_nil(0) of ()
  | {esz:nat}{rsz:nat} json_list_cons(esz + rsz + 1) of (json(esz), json_list(rsz))

and json_entries(int) =
  | json_entries_nil(0) of ()
  | {c:pos}{klen:nat | klen <= c; klen <= STRING_CAP}{vsz:nat}{rsz:nat}
    json_entries_cons(6 * klen + 4 + vsz + rsz) of ([l:agz] $A.arr(byte, l, c), int klen, json(vsz), json_entries(rsz))

#pub vtypedef json_v = [sz:nat] json(sz)
#pub vtypedef json_list_v = [sz:nat] json_list(sz)
#pub vtypedef json_entries_v = [sz:nat] json_entries(sz)

(* Why parse failed, and where: a byte offset in the input.
   UnexpectedEnd     the input ended inside a value (at its end)
   UnexpectedByte    a byte that cannot come here (where it is)
   BadNumber         a number JSON's grammar does not allow: a leading
                     zero, a lone '-', no digit after '.' or the
                     exponent (where the number starts)
   NumberTooLong     a number lexeme longer than STRING_CAP (where it
                     starts)
   StringTooLong     a string longer than STRING_CAP bytes decoded
                     (its opening quote)
   ControlInString   a byte below 0x20 inside a string (where it is)
   BadEscape         a backslash not followed by one of "\/bfnrtu
                     (the backslash)
   BadHex            \u not followed by four hex digits (the backslash)
   InvalidUtf8       a byte that does not begin well-formed UTF-8 in a
                     string (where it is)
   TooDeep           an array or object nested deeper than DEPTH_CAP
                     (its opening bracket)
   TrailingData      parse_text only: something after the value other
                     than whitespace (where it is) *)
#pub datavtype parse_error =
  | UnexpectedEnd of (int)
  | UnexpectedByte of (int)
  | BadNumber of (int)
  | NumberTooLong of (int)
  | StringTooLong of (int)
  | ControlInString of (int)
  | BadEscape of (int)
  | BadHex of (int)
  | InvalidUtf8 of (int)
  | TooDeep of (int)
  | TrailingData of (int)

(* The offset a parse error names; frees it *)
#pub fn parse_error_pos (e: parse_error): int

implement parse_error_pos(e) =
  case+ e of
  | ~UnexpectedEnd(p) => p
  | ~UnexpectedByte(p) => p
  | ~BadNumber(p) => p
  | ~NumberTooLong(p) => p
  | ~StringTooLong(p) => p
  | ~ControlInString(p) => p
  | ~BadEscape(p) => p
  | ~BadHex(p) => p
  | ~InvalidUtf8(p) => p
  | ~TooDeep(p) => p
  | ~TrailingData(p) => p

(* ============================================================
   Free: recursively free a JSON value
   ============================================================ *)

#pub fun json_free {sz:nat} (v: json(sz)): void

#pub fun json_list_free {sz:nat} (lst: json_list(sz)): void

#pub fun json_entries_free {sz:nat} (ents: json_entries(sz)): void

(* A value's parts are smaller than it: .<sz, 0>. for a value and
   .<sz, 1>. for a list or entries decrease on every call *)
fun _free {sz:nat} .<sz, 0>. (v: json(sz)): void =
  case+ v of
  | ~json_null() => ()
  | ~json_bool(_) => ()
  | ~json_num(arr, _, i) => let
      val () = $R.option_discard<int>(i)
    in $A.free<byte>(arr) end
  | ~json_str(arr, _) => $A.free<byte>(arr)
  | ~json_arr(lst) => _free_list(lst)
  | ~json_obj(ents) => _free_entries(ents)

and _free_list {sz:nat} .<sz, 1>. (lst: json_list(sz)): void =
  case+ lst of
  | ~json_list_nil() => ()
  | ~json_list_cons(v, rest) => let
      val () = _free(v)
    in _free_list(rest) end

and _free_entries {sz:nat} .<sz, 1>. (ents: json_entries(sz)): void =
  case+ ents of
  | ~json_entries_nil() => ()
  | ~json_entries_cons(k, _, v, rest) => let
      val () = $A.free<byte>(k)
      val () = _free(v)
    in _free_entries(rest) end

implement json_free(v) = _free(v)

implement json_list_free(lst) = _free_list(lst)

implement json_entries_free(ents) = _free_entries(ents)

(* ============================================================
   Shared by parse and serialize: UTF-8, escapes, number grammar
   ============================================================ *)

fn is_cont (b: int): bool = b >= 128 && b < 192

(* How many bytes the well-formed UTF-8 sequence starting with b0 b1 b2
   b3 has (Unicode 15, table 3-7: no overlong form, no surrogate, nothing
   past U+10FFFF), when avail bytes are there; 0 when there is none *)
fn utf8_len {a:nat} (b0: int, b1: int, b2: int, b3: int, avail: int a)
  : [k:nat | k <= 4; k <= a] int k =
  if b0 < 0 then 0
  else if b0 < 128 then (if avail >= 1 then 1 else 0)
  else if b0 >= 194 && b0 <= 223 then
    (if avail >= 2 then (if is_cont(b1) then 2 else 0) else 0)
  else if b0 >= 224 && b0 <= 239 then let
    val ok1 = (if b0 = 224 then b1 >= 160 && b1 < 192
               else if b0 = 237 then b1 >= 128 && b1 < 160
               else is_cont(b1)): bool
  in
    if avail >= 3 then (if ok1 && is_cont(b2) then 3 else 0) else 0
  end
  else if b0 >= 240 && b0 <= 244 then let
    val ok1 = (if b0 = 240 then b1 >= 144 && b1 < 192
               else if b0 = 244 then b1 >= 128 && b1 < 144
               else is_cont(b1)): bool
  in
    if avail >= 4 then (if ok1 && is_cont(b2) && is_cont(b3) then 4 else 0) else 0
  end
  else 0

(* The letter of the two-byte escape for c, or 0 when it has none *)
fn short_escape (c: int): int =
  if c = 34 then 34          (* \" *)
  else if c = 92 then 92     (* \\ *)
  else if c = 8 then 98      (* \b *)
  else if c = 12 then 102    (* \f *)
  else if c = 10 then 110    (* \n *)
  else if c = 13 then 114    (* \r *)
  else if c = 9 then 116     (* \t *)
  else 0

(* The ASCII hex digit for 0 <= d < 16, lower case *)
fn hex_digit (d: int): [v:nat | v < 256] int v =
  if d < 10 then $AR.low_byte(48 + d) else $AR.low_byte(87 + d)

(* The value of a hex digit, or ~1 *)
fn hex_value (c: int): int =
  if c >= 48 && c <= 57 then c - 48
  else if c >= 97 && c <= 102 then c - 87
  else if c >= 65 && c <= 70 then c - 55
  else ~1

(* JSON's number grammar (RFC 8259 section 6) as a state machine:
   0 start, 1 after '-', 2 a leading 0, 3 integer digits, 4 after '.',
   5 fraction digits, 6 after 'e', 7 after the exponent's sign,
   8 exponent digits. The next state after c, ~1 when c ends the number
   and ~2 for a digit after a leading 0. *)
fn num_step (s: int, c: int): int = let
  val dig = c >= 48 && c <= 57
  val ex = c = 101 || c = 69
in
  if s = 0 then (if c = 45 then 1 else if c = 48 then 2 else if dig then 3 else ~1)
  else if s = 1 then (if c = 48 then 2 else if dig then 3 else ~1)
  else if s = 2 then (if c = 46 then 4 else if ex then 6 else if dig then ~2 else ~1)
  else if s = 3 then (if dig then 3 else if c = 46 then 4 else if ex then 6 else ~1)
  else if s = 4 then (if dig then 5 else ~1)
  else if s = 5 then (if dig then 5 else if ex then 6 else ~1)
  else if s = 6 then (if c = 43 || c = 45 then 7 else if dig then 8 else ~1)
  else if s = 7 then (if dig then 8 else ~1)
  else if s = 8 then (if dig then 8 else ~1)
  else ~1
end

(* Whether a number may end in state s *)
fn num_accepts (s: int): bool = s = 2 || s = 3 || s = 5 || s = 8

(* Whether a[0, len) is a JSON number *)
fn lexeme_ok {l:agz}{c:pos}{len:nat | len <= c}
  (a: !$A.arr(byte, l, c), len: int len): bool = let
  fun loop {i:nat | i <= len} .<len - i>.
    (a: !$A.arr(byte, l, c), i: int i, len: int len, s: int): bool =
    if i >= len then num_accepts(s)
    else let
      val s2 = num_step(s, byte2int0($A.get<byte>(a, i)))
    in if s2 < 0 then false else loop(a, i + 1, len, s2) end
in loop(a, 0, len, 0) end

(* The value of the integer lexeme a[0, len) (an optional '-', then
   digits) when it fits in an int. The digits are summed as a
   non-positive number, so that the minimum int fits. *)
fn lexeme_int {l:agz}{c:pos}{len:nat | len <= c}
  (a: !$A.arr(byte, l, c), len: int len): $R.option(int) = let
  fun loop {i:nat | i <= len} .<len - i>.
    (a: !$A.arr(byte, l, c), i: int i, len: int len, acc: int, neg: bool): $R.option(int) =
    if i >= len then $R.some(if neg then acc else ~acc)
    else let
      val d = byte2int0($A.get<byte>(a, i)) - 48
      val lim = (if neg then 8 else 7): int
    in
      if acc < ~214748364 || (acc = ~214748364 && d > lim) then $R.none()
      else loop(a, i + 1, len, acc * 10 - d, neg)
    end
in
  if len > 0 then
    (if byte2int0($A.get<byte>(a, 0)) = 45 then loop(a, 1, len, 0, true)
     else loop(a, 0, len, 0, false))
  else $R.none()
end

(* ============================================================
   Building values
   ============================================================ *)

(* The number i, as its decimal lexeme *)
#pub fn json_num_of_int (i: int): [sz:nat | sz <= 15] json(sz)

(* How many decimal digits x <= 0 has, counting from k *)
fun ndigits {k:pos | k <= 10} .<10 - k>. (x: int, k: int k): [d:pos | d <= 10] int d =
  if x > ~10 then k
  else if k >= 10 then k
  else ndigits(x / 10, k + 1)

(* The digits of x <= 0 into a[stop, j], last digit at j *)
fun fill_digits {l:agz}{c:pos}{s:nat}{j:int | j < c; j >= ~1} .<j + 1>.
  (a: !$A.arr(byte, l, c), j: int j, stop: int s, x: int): void =
  if j < stop then ()
  else let
    val () = $A.write_byte(a, j, $AR.low_byte(48 + (x / 10) * 10 - x))
  in fill_digits(a, j - 1, stop, x / 10) end

implement json_num_of_int(i) = let
  val neg = i < 0
  val x = (if neg then i else ~i): int
  val d = ndigits(x, 1)
in
  if neg then let
    val a = $A.alloc<byte>(d + 1)
    val () = $A.write_byte(a, 0, 45)
    val () = fill_digits(a, d, 1, x)
  in json_num(a, d + 1, $R.some(i)) end
  else let
    val a = $A.alloc<byte>(d)
    val () = fill_digits(a, d - 1, 0, x)
  in json_num(a, d, $R.some(i)) end
end

(* ============================================================
   Serialize: JSON value → builder (compile-time bounds only)
   ============================================================ *)

(* Writes \u00XX for c < 256 *)
fn put_u00 {n:nat | n + 6 <= $B.BUILDER_CAP}
  (b: !$B.builder(n) >> $B.builder(n + 6), c: int): void = let
  val () = $B.bput(b, "\\u00")
  val () = $B.put_byte(b, hex_digit(c / 16))
in $B.put_byte(b, hex_digit(c - (c / 16) * 16)) end

(* Writes a[0, dlen) escaped for a JSON string: at most 6 output bytes
   for each input byte (\u00XX for a control byte, \ufffd for a byte
   that does not begin UTF-8), so the bytes left, 6 * (dlen - pos),
   bound the output *)
fn emit_escaped {l:agz}{c:pos}{dlen:nat | dlen <= c}{n:nat | n + 6 * dlen <= $B.BUILDER_CAP}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + 6 * dlen] $B.builder(m),
   arr: !$A.arr(byte, l, c), len: int dlen): void = let
  fun loop {pos:nat | pos <= dlen}{p:nat | p + 6 * (dlen - pos) <= $B.BUILDER_CAP} .<dlen - pos>.
    (b: !$B.builder(p) >> [m:nat | p <= m; m <= p + 6 * (dlen - pos)] $B.builder(m),
     arr: !$A.arr(byte, l, c), pos: int pos, len: int dlen): void =
    if pos >= len then ()
    else let
      val c0 = byte2int0($A.get<byte>(arr, pos))
    in
      if c0 < 128 then let
        val e = short_escape(c0)
      in
        if e > 0 then let
          val () = $B.put_byte(b, 92)
          val () = $B.put_byte(b, $AR.low_byte(e))
        in loop(b, arr, pos + 1, len) end
        else if c0 < 32 then let
          val () = put_u00(b, c0)
        in loop(b, arr, pos + 1, len) end
        else let
          val () = $B.put_byte(b, $AR.low_byte(c0))
        in loop(b, arr, pos + 1, len) end
      end
      else let
        val b1 = (if pos + 1 < len then byte2int0($A.get<byte>(arr, pos + 1)) else ~1): int
        val b2 = (if pos + 2 < len then byte2int0($A.get<byte>(arr, pos + 2)) else ~1): int
        val b3 = (if pos + 3 < len then byte2int0($A.get<byte>(arr, pos + 3)) else ~1): int
        val k = utf8_len(c0, b1, b2, b3, len - pos)
      in
        if k = 0 then let
          val () = $B.bput(b, "\\ufffd")
        in loop(b, arr, pos + 1, len) end
        else let
          val () = $B.put_byte(b, $AR.low_byte(c0))
          val () = (if k >= 2 then $B.put_byte(b, $AR.low_byte(b1)) else ())
          val () = (if k >= 3 then $B.put_byte(b, $AR.low_byte(b2)) else ())
          val () = (if k >= 4 then $B.put_byte(b, $AR.low_byte(b3)) else ())
        in loop(b, arr, pos + k, len) end
      end
    end
in loop(b, arr, 0, len) end

(* Writes the lexeme a[0, len), or null when it is not a JSON number *)
fn emit_number {l:agz}{c:pos}{len:pos | len <= c}{n:nat | n + len + 4 <= $B.BUILDER_CAP}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + len + 4] $B.builder(m),
   a: !$A.arr(byte, l, c), len: int len): void = let
  fun loop {i:nat | i <= len}{p:nat | p + len - i <= $B.BUILDER_CAP} .<len - i>.
    (b: !$B.builder(p) >> $B.builder(p + len - i),
     a: !$A.arr(byte, l, c), i: int i, len: int len): void =
    if i >= len then ()
    else let
      val () = $B.put_byte(b, $AR.low_byte(byte2int0($A.get<byte>(a, i))))
    in loop(b, a, i + 1, len) end
in
  if lexeme_ok(a, len) then loop(b, a, 0, len) else $B.bput(b, "null")
end

#pub fun serialize {sz:nat}{n:nat | n + sz <= $B.BUILDER_CAP}
  (v: !json(sz),
   b: !$B.builder(n) >> [m:nat | n <= m; m <= n + sz] $B.builder(m)): void

#pub fun serialize_list {sz:nat}{n:nat | n + sz <= $B.BUILDER_CAP}
  (lst: !json_list(sz),
   b: !$B.builder(n) >> [m:nat | n <= m; m <= n + sz] $B.builder(m),
   first: bool): void

#pub fun serialize_entries {sz:nat}{n:nat | n + sz <= $B.BUILDER_CAP}
  (ents: !json_entries(sz),
   b: !$B.builder(n) >> [m:nat | n <= m; m <= n + sz] $B.builder(m),
   first: bool): void

fun _ser {sz:nat}{n:nat | n + sz <= $B.BUILDER_CAP} .<sz, 0>.
  (v: !json(sz),
   b: !$B.builder(n) >> [m:nat | n <= m; m <= n + sz] $B.builder(m)): void =
  case+ v of
  | json_null() => $B.bput(b, "null")
  | json_bool(t) => (if t then $B.bput(b, "true") else $B.bput(b, "false"))
  | json_num(arr, len, _) => emit_number(b, arr, len)
  | json_str(arr, len) => let
      val () = $B.put_byte(b, 34)
      val () = emit_escaped(b, arr, len)
    in $B.put_byte(b, 34) end
  | json_arr(lst) => let
      val () = $B.put_byte(b, 91)
      val () = _ser_list(lst, b, true)
    in $B.put_byte(b, 93) end
  | json_obj(ents) => let
      val () = $B.put_byte(b, 123)
      val () = _ser_entries(ents, b, true)
    in $B.put_byte(b, 125) end

and _ser_list {sz:nat}{n:nat | n + sz <= $B.BUILDER_CAP} .<sz, 1>.
  (lst: !json_list(sz),
   b: !$B.builder(n) >> [m:nat | n <= m; m <= n + sz] $B.builder(m),
   first: bool): void =
  case+ lst of
  | json_list_nil() => ()
  | json_list_cons(v, rest) => let
      val () = (if ~first then $B.put_byte(b, 44) else ())
      val () = _ser(v, b)
    in _ser_list(rest, b, false) end

and _ser_entries {sz:nat}{n:nat | n + sz <= $B.BUILDER_CAP} .<sz, 1>.
  (ents: !json_entries(sz),
   b: !$B.builder(n) >> [m:nat | n <= m; m <= n + sz] $B.builder(m),
   first: bool): void =
  case+ ents of
  | json_entries_nil() => ()
  | json_entries_cons(k, klen, v, rest) => let
      val () = (if ~first then $B.put_byte(b, 44) else ())
      val () = $B.put_byte(b, 34)
      val () = emit_escaped(b, k, klen)
      val () = $B.put_byte(b, 34)
      val () = $B.put_byte(b, 58)
      val () = _ser(v, b)
    in _ser_entries(rest, b, false) end

implement serialize(v, b) = _ser(v, b)

implement serialize_list(lst, b, first) = _ser_list(lst, b, first)

implement serialize_entries(ents, b, first) = _ser_entries(ents, b, first)

(* ============================================================
   Serialize: JSON value → rope, for a value of any size (a
   builder holds at most BUILDER_CAP bytes). The same text as
   serialize.
   ============================================================ *)

#pub fun serialize_rope {sz:nat} (v: !json(sz), r: !$B.rope): void

fn rope_escaped {l:agz}{c:pos}{dlen:nat | dlen <= c}
  (r: !$B.rope, arr: !$A.arr(byte, l, c), len: int dlen): void = let
  fun loop {pos:nat | pos <= dlen} .<dlen - pos>.
    (r: !$B.rope, arr: !$A.arr(byte, l, c), pos: int pos, len: int dlen): void =
    if pos >= len then ()
    else let
      val c0 = byte2int0($A.get<byte>(arr, pos))
    in
      if c0 < 128 then let
        val e = short_escape(c0)
      in
        if e > 0 then let
          val () = $B.rope_put(r, 92)
          val () = $B.rope_put(r, $AR.low_byte(e))
        in loop(r, arr, pos + 1, len) end
        else if c0 < 32 then let
          val () = $B.rope_bput(r, "\\u00")
          val () = $B.rope_put(r, hex_digit(c0 / 16))
          val () = $B.rope_put(r, hex_digit(c0 - (c0 / 16) * 16))
        in loop(r, arr, pos + 1, len) end
        else let
          val () = $B.rope_put(r, $AR.low_byte(c0))
        in loop(r, arr, pos + 1, len) end
      end
      else let
        val b1 = (if pos + 1 < len then byte2int0($A.get<byte>(arr, pos + 1)) else ~1): int
        val b2 = (if pos + 2 < len then byte2int0($A.get<byte>(arr, pos + 2)) else ~1): int
        val b3 = (if pos + 3 < len then byte2int0($A.get<byte>(arr, pos + 3)) else ~1): int
        val k = utf8_len(c0, b1, b2, b3, len - pos)
      in
        if k = 0 then let
          val () = $B.rope_bput(r, "\\ufffd")
        in loop(r, arr, pos + 1, len) end
        else let
          val () = $B.rope_put(r, $AR.low_byte(c0))
          val () = (if k >= 2 then $B.rope_put(r, $AR.low_byte(b1)) else ())
          val () = (if k >= 3 then $B.rope_put(r, $AR.low_byte(b2)) else ())
          val () = (if k >= 4 then $B.rope_put(r, $AR.low_byte(b3)) else ())
        in loop(r, arr, pos + k, len) end
      end
    end
in loop(r, arr, 0, len) end

fn rope_number {l:agz}{c:pos}{len:pos | len <= c}
  (r: !$B.rope, a: !$A.arr(byte, l, c), len: int len): void = let
  fun loop {i:nat | i <= len} .<len - i>.
    (r: !$B.rope, a: !$A.arr(byte, l, c), i: int i, len: int len): void =
    if i >= len then ()
    else let
      val () = $B.rope_put(r, $AR.low_byte(byte2int0($A.get<byte>(a, i))))
    in loop(r, a, i + 1, len) end
in
  if lexeme_ok(a, len) then loop(r, a, 0, len) else $B.rope_bput(r, "null")
end

fun _rser {sz:nat} .<sz, 0>. (v: !json(sz), r: !$B.rope): void =
  case+ v of
  | json_null() => $B.rope_bput(r, "null")
  | json_bool(t) => (if t then $B.rope_bput(r, "true") else $B.rope_bput(r, "false"))
  | json_num(arr, len, _) => rope_number(r, arr, len)
  | json_str(arr, len) => let
      val () = $B.rope_put(r, 34)
      val () = rope_escaped(r, arr, len)
    in $B.rope_put(r, 34) end
  | json_arr(lst) => let
      val () = $B.rope_put(r, 91)
      val () = _rser_list(lst, r, true)
    in $B.rope_put(r, 93) end
  | json_obj(ents) => let
      val () = $B.rope_put(r, 123)
      val () = _rser_entries(ents, r, true)
    in $B.rope_put(r, 125) end

and _rser_list {sz:nat} .<sz, 1>. (lst: !json_list(sz), r: !$B.rope, first: bool): void =
  case+ lst of
  | json_list_nil() => ()
  | json_list_cons(v, rest) => let
      val () = (if ~first then $B.rope_put(r, 44) else ())
      val () = _rser(v, r)
    in _rser_list(rest, r, false) end

and _rser_entries {sz:nat} .<sz, 1>. (ents: !json_entries(sz), r: !$B.rope, first: bool): void =
  case+ ents of
  | json_entries_nil() => ()
  | json_entries_cons(k, klen, v, rest) => let
      val () = (if ~first then $B.rope_put(r, 44) else ())
      val () = $B.rope_put(r, 34)
      val () = rope_escaped(r, k, klen)
      val () = $B.rope_put(r, 34)
      val () = $B.rope_put(r, 58)
      val () = _rser(v, r)
    in _rser_entries(rest, r, false) end

implement serialize_rope(v, r) = _rser(v, r)

(* ============================================================
   Deserialize: byte buffer → JSON value
   Positions are p <= n, the buffer's size; every scanner recurses on
   n - p, and a value that parses ends past where it began, so the
   value, array and object parsers recurse on (n - p, 0) and (n - p, 1).
   ============================================================ *)

(* Byte at pos, a position in the input *)
fn rd {l:agz}{n:pos}{p:nat | p < n}
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n): int =
  byte2int0($A.read<byte>(src, pos))

(* Byte at pos, or ~1 past the end *)
fn rd_or {l:agz}{n:pos}{p:nat}
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n): int =
  if pos < max then rd(src, pos, max) else ~1

fun skip_ws {l:agz}{n:pos}{p:nat | p <= n} .<n - p>.
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n): [q:int | p <= q; q <= n] int q =
  if pos >= max then pos
  else let val c = rd(src, pos, max) in
    if $AR.eq_int_int(c, 32) || $AR.eq_int_int(c, 9) ||
       $AR.eq_int_int(c, 10) || $AR.eq_int_int(c, 13)
    then skip_ws(src, pos + 1, max)
    else pos
  end

(* --- Strings ---------------------------------------------------- *)

(* A string's bytes as they are decoded, in 4096-byte chunks, newest
   first; the index is how many bytes they hold. A chunk is pushed only
   when it holds some. *)
datavtype chunks(int) =
  | chunks_nil(0) of ()
  | {t:nat}{f:pos | f <= 4096} chunks_cons(t + f) of ([l:agz] $A.arr(byte, l, 4096), int f, chunks(t))

fun chunks_free {t:nat} .<t>. (ch: chunks(t)): void =
  case+ ch of
  | ~chunks_nil() => ()
  | ~chunks_cons(a, _, rest) => let
      val () = $A.free<byte>(a)
    in chunks_free(rest) end

(* a[0, f) into dst[s, s + f) *)
fun copy_chunk {la,ld:agz}{c:pos}{f:nat | f <= 4096}{s:nat | s + f <= c}{i:nat | i <= f} .<f - i>.
  (a: !$A.arr(byte, la, 4096), dst: !$A.arr(byte, ld, c), s: int s, i: int i, f: int f): void =
  if i >= f then ()
  else let
    val () = $A.set<byte>(dst, s + i, $A.get<byte>(a, i))
  in copy_chunk(a, dst, s, i + 1, f) end

(* The chunks, oldest at dst[0], into dst[0, e), freed *)
fun chunks_copy {ld:agz}{c:pos}{t:nat}{e:nat | t <= e; e <= c} .<t>.
  (ch: chunks(t), dst: !$A.arr(byte, ld, c), e: int e): void =
  case+ ch of
  | ~chunks_nil() => ()
  | ~chunks_cons(a, f, rest) => let
      val () = copy_chunk(a, dst, e - f, 0, f)
      val () = $A.free<byte>(a)
    in chunks_copy(rest, dst, e - f) end

(* The string being decoded: the chunk being filled, f bytes so far,
   the full ones, t bytes, and the size, s = t + f. A flat tuple, so
   that appending a byte allocates nothing. *)
vtypedef sbuf(s:int) =
  [lc:agz][f:nat | f <= 4096][t:nat | t + f == s]
  @($A.arr(byte, lc, 4096), int f, chunks(t), int t, int s)

fn sbuf_create (): sbuf(0) = let
  val a = $A.alloc<byte>(4096)
in @(a, 0, chunks_nil(), 0, 0) end

fn sbuf_size {s:nat} (sb: !sbuf(s)): int s = sb.4

fn sbuf_free {s:nat} (sb: sbuf(s)): void = let
  val @(cur, _, done, _, _) = sb
  val () = $A.free<byte>(cur)
in chunks_free(done) end

(* The first k of b0 b1 b2 b3 into a[at, at + k) *)
fn put_k {l:agz}{at:nat}{k:int | 1 <= k; k <= 4; at + k <= 4096}
  (a: !$A.arr(byte, l, 4096), at: int at, k: int k, b0: int, b1: int, b2: int, b3: int): void = let
  val () = $A.write_byte(a, at, $AR.low_byte(b0))
  val () = (if k >= 2 then $A.write_byte(a, at + 1, $AR.low_byte(b1)) else ())
  val () = (if k >= 3 then $A.write_byte(a, at + 2, $AR.low_byte(b2)) else ())
in if k >= 4 then $A.write_byte(a, at + 3, $AR.low_byte(b3)) else () end

(* Appends the first k of b0 b1 b2 b3 *)
fn sbuf_put {s:nat}{k:int | 1 <= k; k <= 4}
  (sb: sbuf(s), k: int k, b0: int, b1: int, b2: int, b3: int): sbuf(s + k) = let
  val @(cur, f, done, t, s) = sb
in
  if f + k > 4096 then let
    val nc = $A.alloc<byte>(4096)
    val () = put_k(nc, 0, k, b0, b1, b2, b3)
  in @(nc, k, chunks_cons(cur, f, done), t + f, s + k) end
  else let
    val () = put_k(cur, f, k, b0, b1, b2, b3)
  in @(cur, f + k, done, t, s + k) end
end

(* Appends the byte x *)
fn sbuf_byte {s:nat} (sb: sbuf(s), x: byte): sbuf(s + 1) = let
  val @(cur, f, done, t, s) = sb
in
  if f >= 4096 then let
    val nc = $A.alloc<byte>(4096)
    val () = $A.set<byte>(nc, 0, x)
  in @(nc, 1, chunks_cons(cur, f, done), t + f, s + 1) end
  else let
    val () = $A.set<byte>(cur, f, x)
  in @(cur, f + 1, done, t, s + 1) end
end

(* Appends src[p, p + k) *)
fun sbuf_src {l:agz}{n:pos}{p:nat}{k:nat | p + k <= n}{s:nat} .<k>.
  (sb: sbuf(s), src: !$A.borrow(byte, l, n), p: int p, k: int k): sbuf(s + k) =
  if k <= 0 then sb
  else sbuf_src(sbuf_byte(sb, $A.read<byte>(src, p)), src, p + 1, k - 1)

(* Copies the run of plain bytes at p (0x20 to 0x7f, but '"' and '\\')
   into cur from f on, as far as cur has room and at most room bytes:
   where the run stopped, in the input and in cur *)
fun plain_run {l,lc:agz}{n:pos}{p:nat | p <= n}{f:nat | f <= 4096}{room:nat} .<n - p>.
  (src: !$A.borrow(byte, l, n), p: int p, max: int n,
   cur: !$A.arr(byte, lc, 4096), f: int f, room: int room)
  : [q:int | p <= q; q <= n][g:int | f <= g; g <= 4096; g - f == q - p; q - p <= room] @(int q, int g) =
  if p >= max then @(p, f)
  else if f >= 4096 then @(p, f)
  else if room <= 0 then @(p, f)
  else let
    val c = rd(src, p, max)
  in
    if c >= 32 && c < 128 && c <> 34 && c <> 92 then let
      val () = $A.set<byte>(cur, f, $A.read<byte>(src, p))
    in plain_run(src, p + 1, max, cur, f + 1, room - 1) end
    else @(p, f)
  end

(* The chunk being filled pushed onto the full ones, or freed when
   empty *)
fn chunks_close {lc:agz}{f:nat | f <= 4096}{t:nat}
  (cur: $A.arr(byte, lc, 4096), f: int f, done: chunks(t)): chunks(t + f) =
  if f > 0 then chunks_cons(cur, f, done)
  else let val () = $A.free<byte>(cur) in done end

(* The decoded string, in an array of exactly its size (one byte for
   the empty string, which alloc needs) *)
fn sbuf_finish {s:nat | s <= STRING_CAP} (sb: sbuf(s))
  : [l:agz][c:pos | s <= c; c <= STRING_CAP] @($A.arr(byte, l, c), int s) = let
  val @(cur, f, done, t, _) = sb
  val all = chunks_close(cur, f, done)
  val s = t + f
in
  if s > 0 then let
    val dst = $A.alloc<byte>(s)
    val () = chunks_copy(all, dst, s)
  in @(dst, s) end
  else let
    val dst = $A.alloc<byte>(1)
    val () = chunks_copy(all, dst, 0)
  in @(dst, 0) end
end

(* Appends the UTF-8 of code point u (below 0x110000, not a surrogate)
   when it fits under STRING_CAP *)
fn sbuf_code {s:nat | s <= STRING_CAP} (sb: sbuf(s), u: int)
  : @(bool, [s2:nat | s2 <= STRING_CAP] sbuf(s2)) = let
  val k = (if u < 128 then 1 else if u < 2048 then 2 else if u < 65536 then 3 else 4)
    : [k:int | 1 <= k; k <= 4] int k
in
  if sbuf_size(sb) + k > _STRING_CAP then @(false, sb)
  else if k = 1 then @(true, sbuf_put(sb, 1, u, 0, 0, 0))
  else if k = 2 then
    @(true, sbuf_put(sb, 2, 192 + u / 64, 128 + u - (u / 64) * 64, 0, 0))
  else if k = 3 then
    @(true, sbuf_put(sb, 3, 224 + u / 4096, 128 + (u / 64) - (u / 4096) * 64,
                     128 + u - (u / 64) * 64, 0))
  else
    @(true, sbuf_put(sb, 4, 240 + u / 262144, 128 + (u / 4096) - (u / 262144) * 64,
                     128 + (u / 64) - (u / 4096) * 64, 128 + u - (u / 64) * 64))
end

(* The value of the four hex digits at p, or ~1 when one is not a hex
   digit *)
fn hex4 {l:agz}{n:pos}{p:nat | p + 4 <= n}
  (src: !$A.borrow(byte, l, n), p: int p, max: int n): int = let
  val h0 = hex_value(rd(src, p, max))
  val h1 = hex_value(rd(src, p + 1, max))
  val h2 = hex_value(rd(src, p + 2, max))
  val h3 = hex_value(rd(src, p + 3, max))
in
  if h0 < 0 || h1 < 0 || h2 < 0 || h3 < 0 then ~1
  else h0 * 4096 + h1 * 256 + h2 * 16 + h3
end

(* Whether one of the bytes from p to the end, fewer than four, is not
   a hex digit *)
fun hex_tail_bad {l:agz}{n:pos}{p:nat | p <= n} .<n - p>.
  (src: !$A.borrow(byte, l, n), p: int p, max: int n): bool =
  if p >= max then false
  else if hex_value(rd(src, p, max)) < 0 then true
  else hex_tail_bad(src, p + 1, max)

(* Parses a string whose opening quote is at o; its body starts at
   pos. RFC 8259 section 7: every escape is decoded, a surrogate pair
   joined into one code point; a lone surrogate (\uD800-\uDFFF not in
   a pair) becomes U+FFFD, as Go's encoding/json does and as the
   WHATWG TextEncoder does when JavaScript turns such a string into
   UTF-8 (serde_json rejects it when reading a String; Python keeps it,
   and WTF-8 would keep its bytes, but neither is UTF-8 and the WTF-8
   spec forbids it in interchange). Raw bytes must be well-formed UTF-8
   (section 8.1) and not below 0x20. *)
fun str_loop {l:agz}{n:pos}{o:nat}{p:nat | o < p; p <= n}{s:nat | s <= STRING_CAP} .<n - p>.
  (src: !$A.borrow(byte, l, n), max: int n, o: int o, pos: int p, sb: sbuf(s))
  : $R.result([q:int | o < q; q <= n] @([s2:nat | s2 <= STRING_CAP] sbuf(s2), int q), parse_error) =
  if pos >= max then let
    val () = sbuf_free(sb)
  in $R.err(UnexpectedEnd(pos)) end
  else let
    val c = rd(src, pos, max)
  in
    if c = 34 then $R.ok(@(sb, pos + 1))
    else if c < 32 then let
      val () = sbuf_free(sb)
    in $R.err(ControlInString(pos)) end
    else if c = 92 then
      (if pos + 1 >= max then let
         val () = sbuf_free(sb)
       in $R.err(UnexpectedEnd(pos + 1)) end
       else let
         val e = rd(src, pos + 1, max)
         val ch = (if e = 34 then 34 else if e = 92 then 92 else if e = 47 then 47
                   else if e = 98 then 8 else if e = 102 then 12 else if e = 110 then 10
                   else if e = 114 then 13 else if e = 116 then 9 else ~1): int
       in
         if ch >= 0 then
           (if sbuf_size(sb) + 1 > _STRING_CAP then let
              val () = sbuf_free(sb)
            in $R.err(StringTooLong(o)) end
            else str_loop(src, max, o, pos + 2, sbuf_put(sb, 1, ch, 0, 0, 0)))
         else if e <> 117 then let
           val () = sbuf_free(sb)
         in $R.err(BadEscape(pos)) end
         else if pos + 6 > max then let
           val bad = hex_tail_bad(src, pos + 2, max)
           val () = sbuf_free(sb)
         in
           if bad then $R.err(BadHex(pos)) else $R.err(UnexpectedEnd(max))
         end
         else let
           val u = hex4(src, pos + 2, max)
         in
           if u < 0 then let
             val () = sbuf_free(sb)
           in $R.err(BadHex(pos)) end
           else let
             (* A high surrogate joins the low one in the escape right
                after it; any other surrogate stands alone *)
             val @(cp, adv) = (
               if u >= 55296 && u < 56320 then
                 (if pos + 12 <= max then let
                    val lo = (if rd(src, pos + 6, max) = 92 && rd(src, pos + 7, max) = 117
                              then hex4(src, pos + 8, max) else ~1): int
                  in
                    if lo >= 56320 && lo < 57344
                    then @(65536 + (u - 55296) * 1024 + (lo - 56320), 12)
                    else @(65533, 6)
                  end
                  else @(65533, 6))
               else if u >= 56320 && u < 57344 then @(65533, 6)
               else @(u, 6)): @(int, [a:int | a >= 6; p + a <= n] int a)
             val @(fits, sb) = sbuf_code(sb, cp)
           in
             if fits then str_loop(src, max, o, pos + adv, sb)
             else let
               val () = sbuf_free(sb)
             in $R.err(StringTooLong(o)) end
           end
         end
       end)
    else if c < 128 then
      (if sbuf_size(sb) + 1 > _STRING_CAP then let
         val () = sbuf_free(sb)
       in $R.err(StringTooLong(o)) end
       else let
         (* A run of plain bytes goes straight into the chunk *)
         val @(cur, f, done, t, sz) = sb
         val @(q, g) = plain_run(src, pos, max, cur, f, _STRING_CAP - sz)
         val sb = @(cur, g, done, t, sz + (g - f))
       in
         if q > pos then str_loop(src, max, o, q, sb)
         else str_loop(src, max, o, pos + 1, sbuf_byte(sb, $A.read<byte>(src, pos)))
       end)
    else let
      val k = utf8_len(c, rd_or(src, pos + 1, max), rd_or(src, pos + 2, max),
                       rd_or(src, pos + 3, max), max - pos)
    in
      if k = 0 then let
        val () = sbuf_free(sb)
      in $R.err(InvalidUtf8(pos)) end
      else if sbuf_size(sb) + k > _STRING_CAP then let
        val () = sbuf_free(sb)
      in $R.err(StringTooLong(o)) end
      else str_loop(src, max, o, pos + k, sbuf_src(sb, src, pos, k))
    end
  end

(* The string whose opening quote is at o: its bytes in an array of
   exactly their size, and its end, past the closing quote *)
fn parse_string {l:agz}{n:pos}{o:nat | o < n}
  (src: !$A.borrow(byte, l, n), o: int o, max: int n)
  : $R.result([lr:agz][c:pos][d:nat | d <= c; d <= STRING_CAP][q:int | o < q; q <= n]
              @($A.arr(byte, lr, c), int d, int q), parse_error) =
  case+ str_loop(src, max, o, o + 1, sbuf_create()) of
  | ~$R.ok(@(sb, q)) => let
      val @(arr, d) = sbuf_finish(sb)
    in $R.ok(@(arr, d, q)) end
  | ~$R.err(e) => $R.err(e)

(* --- Numbers ---------------------------------------------------- *)

(* The state the number reaches from state s at pos, and where it ends *)
fun num_scan {l:agz}{n:pos}{p:nat | p <= n} .<n - p>.
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n, s: int)
  : @(int, [q:int | p <= q; q <= n] int q) =
  if pos >= max then @(s, pos)
  else let
    val s2 = num_step(s, rd(src, pos, max))
  in
    if s2 = ~1 then @(s, pos)
    else if s2 < 0 then @(s2, pos)
    else num_scan(src, pos + 1, max, s2)
  end

(* src[s, s + len) into dst[0, len) *)
fun copy_src {l,ld:agz}{n:pos}{c:pos}{s:nat}{len:nat | s + len <= n; len <= c}{i:nat | i <= len} .<len - i>.
  (src: !$A.borrow(byte, l, n), s: int s, dst: !$A.arr(byte, ld, c), i: int i, len: int len): void =
  if i >= len then ()
  else let
    val () = $A.set<byte>(dst, i, $A.read<byte>(src, s + i))
  in copy_src(src, s, dst, i + 1, len) end

(* The number starting at p, whose first byte is '-' or a digit *)
fn parse_number {l:agz}{n:pos}{s0:nat}{p:nat | s0 <= p; p < n}
  (src: !$A.borrow(byte, l, n), p: int p, max: int n)
  : $R.result(@(json_v, [q:int | s0 < q; q <= n] int q), parse_error) = let
  val @(s, e) = num_scan(src, p + 1, max, num_step(0, rd(src, p, max)))
  val len = e - p
in
  if s < 0 || ~num_accepts(s) then $R.err(BadNumber(p))
  else if len > _STRING_CAP then $R.err(NumberTooLong(p))
  else let
    val a = $A.alloc<byte>(len)
    val () = copy_src(src, p, a, 0, len)
    val i = (if s <= 3 then lexeme_int(a, len) else $R.none()): $R.option(int)
  in $R.ok(@(json_num(a, len, i), e)) end
end

(* --- Values ----------------------------------------------------- *)

#pub fun parse {l:agz}{n:pos}{p:nat | p <= n}
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n
  ): $R.result(@(json_v, [q:nat] int q), parse_error)

#pub fun parse_text {l:agz}{n:pos}
  (src: !$A.borrow(byte, l, n), max: int n): $R.result(json_v, parse_error)

(* lst reversed onto acc *)
fun reverse_list {s,a:nat} .<s>.
  (lst: json_list(s), acc: json_list(a)): json_list(s + a) =
  case+ lst of
  | ~json_list_nil() => acc
  | ~json_list_cons(v, rest) => reverse_list(rest, json_list_cons(v, acc))

fun reverse_entries {s,a:nat} .<s>.
  (ents: json_entries(s), acc: json_entries(a)): json_entries(s + a) =
  case+ ents of
  | ~json_entries_nil() => acc
  | ~json_entries_cons(k, kl, v, rest) => reverse_entries(rest, json_entries_cons(k, kl, v, acc))

(* Whether src[p, p + 4) is c0 c1 c2 c3 *)
fn at4 {l:agz}{n:pos}{p:nat | p + 4 <= n}
  (src: !$A.borrow(byte, l, n), p: int p, max: int n, c0: int, c1: int, c2: int, c3: int): bool =
  $AR.eq_int_int(rd(src, p, max), c0) && $AR.eq_int_int(rd(src, p + 1, max), c1) &&
  $AR.eq_int_int(rd(src, p + 2, max), c2) && $AR.eq_int_int(rd(src, p + 3, max), c3)

(* The value at pos (after blanks), and its end, which is past pos;
   depth is how many arrays and objects enclose it *)
fun _parse {l:agz}{n:pos}{p:nat | p <= n} .<n - p, 0>.
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n, depth: int
  ): $R.result(@(json_v, [q:int | p < q; q <= n] int q), parse_error) = let
  val [q1:int] p1 = skip_ws(src, pos, max)
in
  if p1 >= max then $R.err(UnexpectedEnd(p1))
  else let val c = rd(src, p1, max) in
    (* null *)
    if $AR.eq_int_int(c, 110) then
      if p1 + 4 <= max then
        (if at4(src, p1, max, 110, 117, 108, 108) then $R.ok(@(json_null(), p1 + 4))
         else $R.err(UnexpectedByte(p1)))
      else $R.err(UnexpectedEnd(max))
    (* true *)
    else if $AR.eq_int_int(c, 116) then
      if p1 + 4 <= max then
        (if at4(src, p1, max, 116, 114, 117, 101) then $R.ok(@(json_bool(true), p1 + 4))
         else $R.err(UnexpectedByte(p1)))
      else $R.err(UnexpectedEnd(max))
    (* false *)
    else if $AR.eq_int_int(c, 102) then
      if p1 + 5 <= max then
        (if at4(src, p1 + 1, max, 97, 108, 115, 101) then $R.ok(@(json_bool(false), p1 + 5))
         else $R.err(UnexpectedByte(p1)))
      else $R.err(UnexpectedEnd(max))
    (* string *)
    else if $AR.eq_int_int(c, 34) then
      (case+ parse_string(src, p1, max) of
       | ~$R.ok(@(arr, len, ep)) => $R.ok(@(json_str(arr, len), ep))
       | ~$R.err(e) => $R.err(e))
    (* array *)
    else if $AR.eq_int_int(c, 91) then
      (if depth >= _DEPTH_CAP then $R.err(TooDeep(p1))
       else case+ _parse_array{l}{n}{q1+1}(src, p1 + 1, max, json_list_nil(), true, depth + 1) of
       | ~$R.ok(@(lst, ep)) => $R.ok(@(json_arr(reverse_list(lst, json_list_nil())), ep))
       | ~$R.err(e) => $R.err(e))
    (* object *)
    else if $AR.eq_int_int(c, 123) then
      (if depth >= _DEPTH_CAP then $R.err(TooDeep(p1))
       else case+ _parse_object{l}{n}{q1+1}(src, p1 + 1, max, json_entries_nil(), true, depth + 1) of
       | ~$R.ok(@(ents, ep)) => $R.ok(@(json_obj(reverse_entries(ents, json_entries_nil())), ep))
       | ~$R.err(e) => $R.err(e))
    (* number *)
    else if (c >= 48 && c <= 57) || $AR.eq_int_int(c, 45) then parse_number{l}{n}{p}(src, p1, max)
    else $R.err(UnexpectedByte(p1))
  end
end

(* The elements from pos to the closing ], onto acc (newest first):
   first, a value or ]; after one, a comma and a value, or ] *)
and _parse_array {l:agz}{n:pos}{s:nat}{p:nat | s <= p; p <= n} .<n - p, 1>.
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n,
   acc: json_list_v, first: bool, depth: int
  ): $R.result(@(json_list_v, [q:int | s <= q; q <= n] int q), parse_error) = let
  val p1 = skip_ws(src, pos, max)
in
  if p1 >= max then let
    val () = json_list_free(acc)
  in $R.err(UnexpectedEnd(p1)) end
  else let val c = rd(src, p1, max) in
    if $AR.eq_int_int(c, 93) then $R.ok(@(acc, p1 + 1)) (* ] *)
    else if ~first && ~($AR.eq_int_int(c, 44)) then let
      val () = json_list_free(acc)
    in $R.err(UnexpectedByte(p1)) end
    else let
      val pv = (if first then p1 else p1 + 1): [v:int | p <= v; v <= n] int v
    in
      case+ _parse(src, pv, max, depth) of
      | ~$R.ok(@(v, ep)) => _parse_array{l}{n}{s}(src, ep, max, json_list_cons(v, acc), false, depth)
      | ~$R.err(e) => let
          val () = json_list_free(acc)
        in $R.err(e) end
    end
  end
end

(* The entries from pos to the closing }, onto acc (newest first):
   first, a key or }; after one, a comma and a key, or } *)
and _parse_object {l:agz}{n:pos}{s:nat}{p:nat | s <= p; p <= n} .<n - p, 1>.
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n,
   acc: json_entries_v, first: bool, depth: int
  ): $R.result(@(json_entries_v, [q:int | s <= q; q <= n] int q), parse_error) = let
  val p1 = skip_ws(src, pos, max)
in
  if p1 >= max then let
    val () = json_entries_free(acc)
  in $R.err(UnexpectedEnd(p1)) end
  else let val c = rd(src, p1, max) in
    if $AR.eq_int_int(c, 125) then $R.ok(@(acc, p1 + 1)) (* } *)
    else if ~first && ~($AR.eq_int_int(c, 44)) then let
      val () = json_entries_free(acc)
    in $R.err(UnexpectedByte(p1)) end
    else let
      val pk = skip_ws(src, (if first then p1 else p1 + 1): [v:int | p <= v; v <= n] int v, max)
    in
      if pk >= max then let
        val () = json_entries_free(acc)
      in $R.err(UnexpectedEnd(pk)) end
      else if ~($AR.eq_int_int(rd(src, pk, max), 34)) then let
        val () = json_entries_free(acc)
      in $R.err(UnexpectedByte(pk)) end
      else
        case+ parse_string(src, pk, max) of
        | ~$R.err(e) => let
            val () = json_entries_free(acc)
          in $R.err(e) end
        | ~$R.ok(@(karr, klen, kep)) => let
            val p3 = skip_ws(src, kep, max)
          in
            if p3 >= max then let
              val () = $A.free<byte>(karr)
              val () = json_entries_free(acc)
            in $R.err(UnexpectedEnd(p3)) end
            else if $AR.eq_int_int(rd(src, p3, max), 58) then (* : *)
              (case+ _parse(src, p3 + 1, max, depth) of
               | ~$R.ok(@(v, vep)) =>
                   _parse_object{l}{n}{s}(src, vep, max, json_entries_cons(karr, klen, v, acc), false, depth)
               | ~$R.err(e) => let
                   val () = $A.free<byte>(karr)
                   val () = json_entries_free(acc)
                 in $R.err(e) end)
            else let
              val () = $A.free<byte>(karr)
              val () = json_entries_free(acc)
            in $R.err(UnexpectedByte(p3)) end
          end
    end
  end
end

implement parse (src, pos, max) =
  case+ _parse(src, pos, max, 0) of
  | ~$R.ok(@(v, q)) => $R.ok(@(v, q))
  | ~$R.err(e) => $R.err(e)

implement parse_text (src, max) =
  case+ _parse(src, 0, max, 0) of
  | ~$R.ok(@(v, q)) => let
      val t = skip_ws(src, q, max)
    in
      if t < max then let
        val () = json_free(v)
      in $R.err(TrailingData(t)) end
      else $R.ok(v)
    end
  | ~$R.err(e) => $R.err(e)

$UNITTEST.run begin

(* Whether a[i, k) and b[i, k) hold the same bytes *)
fun same_from {la,lb:agz}{k:nat | k <= $B.BUILDER_CAP}{i:nat | i <= k} .<k - i>.
  (a: !$A.arr(byte, la, $B.BUILDER_CAP), b: !$A.arr(byte, lb, $B.BUILDER_CAP), i: int i, k: int k): bool =
  if i >= k then true
  else if byte2int0($A.get<byte>(a, i)) = byte2int0($A.get<byte>(b, i)) then same_from(a, b, i + 1, k)
  else false

fn same_arrs {la,lb:agz}{an,bn:nat | an <= $B.BUILDER_CAP; bn <= $B.BUILDER_CAP}
  (a: $A.arr(byte, la, $B.BUILDER_CAP), an: int an, b: $A.arr(byte, lb, $B.BUILDER_CAP), bn: int bn): bool = let
  val ok = (if an = bn then same_from(a, b, 0, an) else false): bool
  val () = $A.free<byte>(a)
  val () = $A.free<byte>(b)
in ok end

(* v serializes to exactly s *)
fn serializes_to {sz:nat | sz <= 524288}{sn:nat | sn <= 256}
  (v: json(sz), s: string sn): bool = let
  val o = $B.create()
  val () = serialize(v, o)
  val () = json_free(v)
  val @(oa, on) = $B.to_arr(o)
  val e = $B.create()
  val () = $B.bput(e, s)
  val @(ea, en) = $B.to_arr(e)
in same_arrs(oa, on, ea, en) end

(* The kind of the value s parses to as a whole text (0 null, 1 bool,
   2 number, 3 string, 4 array, 5 object; ~1 when it does not parse),
   and the int (~1 for a number without one) or the string's length *)
fn parse_kind {sn:pos | sn <= 256} (s: string sn): @(int, int) = let
  val b = $B.create()
  val () = $B.bput(b, s)
  val @(arr, n) = $B.to_arr(b)
  val @(f, bv) = $A.freeze<byte>(arr)
  val @(text, rest) = $A.borrow_split<byte>(f, bv, n)
  val r = (case+ parse_text(text, n) of
    | ~$R.ok(v) => let
        val kv = (case+ v of
          | json_null() => @(0, 0)
          | json_bool(_) => @(1, 0)
          | json_num(_, _, $R.some(i)) => @(2, i)
          | json_num(_, _, $R.none()) => @(2, ~1)
          | json_str(_, len) => @(3, len)
          | json_arr(_) => @(4, 0)
          | json_obj(_) => @(5, 0)): @(int, int)
        val () = json_free(v)
      in kv end
    | ~$R.err(e) => let val _ = parse_error_pos(e) in @(~1, 0) end): @(int, int)
  val () = $A.drop<byte>(f, $A.borrow_join<byte>(f, text, rest))
  val () = $A.free<byte>($A.thaw<byte>(f))
in r end

fn test_serialize_null (): bool = serializes_to(json_null(), "null")

fn test_serialize_true (): bool = serializes_to(json_bool(true), "true")

fn test_serialize_int (): bool = serializes_to(json_num_of_int(42), "42")

fn test_serialize_int_min (): bool = serializes_to(json_num_of_int(~2147483647 - 1), "-2147483648")

fn test_serialize_zero (): bool = serializes_to(json_num_of_int(0), "0")

fn test_serialize_string (): bool = let
  val s = $A.alloc<byte>(2)
  val () = $A.write_byte(s, 0, 104) (* h *)
  val () = $A.write_byte(s, 1, 105) (* i *)
in serializes_to(json_str(s, 2), "\"hi\"") end

fn test_roundtrip_null (): bool = let
  val @(k, _) = parse_kind("null")
in k = 0 end

fn test_roundtrip_int (): bool = let
  val @(k, v) = parse_kind("123")
in if k = 2 then v = 123 else false end

fn test_roundtrip_string (): bool = let
  val @(k, len) = parse_kind("\"hello\"")
in if k = 3 then len = 5 else false end

fn test_roundtrip_array (): bool = let
  val @(k, _) = parse_kind("[1,2,3]")
in k = 4 end

fn test_roundtrip_object (): bool = let
  val @(k, _) = parse_kind("{\"a\":1,\"b\":true}")
in k = 5 end

fn test_roundtrip_nested (): bool = let
  val @(k, _) = parse_kind("{\"x\":[1,{\"y\":null}],\"z\":false}")
in k = 5 end

fn test_fraction_has_no_int (): bool = let
  val @(k, v) = parse_kind("1.5")
in if k = 2 then v = ~1 else false end

fn test_missing_comma_rejected (): bool = let
  val @(k, _) = parse_kind("[1 2]")
in k = ~1 end

end
