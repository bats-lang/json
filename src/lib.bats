(* json -- JSON serialization and deserialization *)
(* Safe: no $UNSAFE, no $extfcall *)
(* Size-indexed types: serialize has zero runtime bounds checks. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use builder as B
#use result as R

(* ============================================================
   JSON value type — size-indexed for compile-time bounds
   The int index is the max serialized bytes.
   ============================================================ *)

#pub datavtype json(int) =
  | json_null(4) of ()
  | json_bool(5) of (bool)
  | json_int(21) of (int)
  | {dlen:nat | dlen <= 4096} json_str(8194) of ([l:agz] $A.arr(byte, l, 4096), int dlen)
  | {sz:nat} json_arr(sz + 2) of (json_list(sz))
  | {sz:nat} json_obj(sz + 2) of (json_entries(sz))

and json_list(int) =
  | json_list_nil(0) of ()
  | {esz:nat}{rsz:nat} json_list_cons(esz + rsz + 1) of (json(esz), json_list(rsz))

and json_entries(int) =
  | json_entries_nil(0) of ()
  | {klen:nat | klen <= 4096}{vsz:nat}{rsz:nat} json_entries_cons(8196 + vsz + rsz) of ([l:agz] $A.arr(byte, l, 4096), int klen, json(vsz), json_entries(rsz))

#pub vtypedef json_v = [sz:nat] json(sz)
#pub vtypedef json_list_v = [sz:nat] json_list(sz)
#pub vtypedef json_entries_v = [sz:nat] json_entries(sz)

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
  | ~json_int(_) => ()
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
   Serialize: JSON value → builder (compile-time bounds only)
   ============================================================ *)

(* Helper: write a byte array to builder, escaping for JSON strings.
   Each input byte produces at most 2 output bytes (escape sequences),
   so the bytes left, 2 * (dlen - pos), bound the output: at most
   2 * 4096 = 8192. *)
fn emit_escaped {l:agz}{n:nat | n + 8192 <= $B.BUILDER_CAP}
    {dlen:nat | dlen <= 4096}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + 8192] $B.builder(m),
   arr: !$A.arr(byte, l, 4096), len: int dlen): void = let
  fun loop {l2:agz}{pos:nat | pos <= dlen}{p:nat | p + 2 * (dlen - pos) <= $B.BUILDER_CAP} .<dlen - pos>.
    (b: !$B.builder(p) >> [m:nat | p <= m; m <= p + 2 * (dlen - pos)] $B.builder(m),
     arr: !$A.arr(byte, l2, 4096), pos: int pos, len: int dlen): void =
    if pos >= len then ()
    else let
      val c = byte2int0($A.get<byte>(arr, pos))
    in
      if $AR.eq_int_int(c, 34) then let (* " *)
        val () = $B.put_byte(b, 92) val () = $B.put_byte(b, 34)
      in loop(b, arr, pos + 1, len) end
      else if $AR.eq_int_int(c, 92) then let (* \ *)
        val () = $B.put_byte(b, 92) val () = $B.put_byte(b, 92)
      in loop(b, arr, pos + 1, len) end
      else if $AR.eq_int_int(c, 10) then let (* \n *)
        val () = $B.put_byte(b, 92) val () = $B.put_byte(b, 110)
      in loop(b, arr, pos + 1, len) end
      else if $AR.eq_int_int(c, 9) then let (* \t *)
        val () = $B.put_byte(b, 92) val () = $B.put_byte(b, 116)
      in loop(b, arr, pos + 1, len) end
      else if $AR.eq_int_int(c, 13) then let (* \r *)
        val () = $B.put_byte(b, 92) val () = $B.put_byte(b, 114)
      in loop(b, arr, pos + 1, len) end
      else let
        val () = $B.put_byte(b, $AR.low_byte(c))
      in loop(b, arr, pos + 1, len) end
    end
in loop(b, arr, 0, len) end

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
  | json_int(n) => $B.put_int(b, n)
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
   Deserialize: byte buffer → JSON value
   Positions are p <= n, the buffer's size; every scanner recurses on
   n - p, and a value that parses ends past where it began, so the
   value, array and object parsers recurse on (n - p, 0) and (n - p, 1).
   ============================================================ *)

(* Byte at pos, a position in the input *)
fn rd {l:agz}{n:pos}{p:nat | p < n}
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n): int =
  byte2int0($A.read<byte>(src, pos))

fun skip_ws {l:agz}{n:pos}{p:nat | p <= n} .<n - p>.
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n): [q:int | p <= q; q <= n] int q =
  if pos >= max then pos
  else let val c = rd(src, pos, max) in
    if $AR.eq_int_int(c, 32) || $AR.eq_int_int(c, 9) ||
       $AR.eq_int_int(c, 10) || $AR.eq_int_int(c, 13)
    then skip_ws(src, pos + 1, max)
    else pos
  end

(* Parses the body of a string after its opening quote. The last
   component says whether the closing quote was found; an unterminated
   string (end of input, or longer than the 4096-byte buffer) is not. *)
fn parse_string {l:agz}{n:pos}{p:nat | p <= n}
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n)
  : @([ls:agz] $A.arr(byte, ls, 4096), [dlen:nat | dlen <= 4096] int dlen,
      [q:int | p <= q; q <= n] int q, bool) = let
  val out = $A.alloc<byte>(4096)
  fun loop {lo:agz}{pp:nat | p <= pp; pp <= n}{opos:nat | opos <= 4096} .<n - pp>.
    (src: !$A.borrow(byte, l, n), pos: int pp, max: int n,
     out: !$A.arr(byte, lo, 4096), opos: int opos)
    : @([r:nat | r <= 4096] int r, [q:int | p <= q; q <= n] int q, bool) =
    if pos >= max then @(opos, pos, false) (* end of input *)
    else let val c = rd(src, pos, max) in
      if $AR.eq_int_int(c, 34) then @(opos, pos + 1, true) (* closing " *)
      else if opos >= 4095 then @(opos, pos, false) (* buffer full *)
      else if $AR.eq_int_int(c, 92) then (* backslash escape *)
        if pos + 1 >= max then @(opos, pos, false)
        else let
          val c2 = rd(src, pos + 1, max)
          val ec = (if $AR.eq_int_int(c2, 110) then 10        (* \n *)
                    else if $AR.eq_int_int(c2, 116) then 9    (* \t *)
                    else if $AR.eq_int_int(c2, 114) then 13   (* \r *)
                    else if $AR.eq_int_int(c2, 34) then 34    (* \" *)
                    else if $AR.eq_int_int(c2, 92) then 92    (* \\ *)
                    else c2): int
          val () = $A.set<byte>(out, opos, int2byte0(ec))
        in loop(src, pos + 2, max, out, opos + 1) end
      else let
        val () = $A.set<byte>(out, opos, int2byte0(c))
      in loop(src, pos + 1, max, out, opos + 1) end
    end
  val @(olen, epos, closed) = loop(src, pos, max, out, 0)
in @(out, olen, epos, closed) end

(* The digits at pos, after acc (the digits so far, as a non-positive
   number, so that the minimum int fits): whether the number fits in an
   int, its value, and its end *)
fun parse_int {l:agz}{n:pos}{p:nat | p <= n} .<n - p>.
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n, acc: int, neg: bool)
  : @(bool, int, [q:int | p <= q; q <= n] int q) =
  if pos >= max then @(true, (if neg then acc else ~acc), pos)
  else let
    val c = rd(src, pos, max)
  in
    if c >= 48 && c <= 57 then let
      val d = c - 48
      val lim = (if neg then 8 else 7): int
    in
      if acc < ~214748364 || (acc = ~214748364 && d > lim) then @(false, 0, pos)
      else parse_int(src, pos + 1, max, acc * 10 - d, neg)
    end
    else @(true, (if neg then acc else ~acc), pos)
  end

#pub fun parse {l:agz}{n:pos}{p:nat | p <= n}
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n
  ): $R.result(@(json_v, [q:nat] int q), int)

fn skip_comma {l:agz}{n:pos}{p:nat | p <= n}
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n, first: bool): [q:int | p <= q; q <= n] int q =
  if first then pos
  else let
    val pc = skip_ws(src, pos, max)
  in
    if pc < max then
      (if $AR.eq_int_int(rd(src, pc, max), 44) then skip_ws(src, pc + 1, max) else pc)
    else pc
  end

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

(* The value at pos (after blanks), and its end, which is past pos *)
fun _parse {l:agz}{n:pos}{p:nat | p <= n} .<n - p, 0>.
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n
  ): $R.result(@(json_v, [q:int | p < q; q <= n] int q), int) = let
  val [q1:int] p1 = skip_ws(src, pos, max)
in
  if p1 >= max then $R.err(p1)
  else let val c = rd(src, p1, max) in
    (* null *)
    if $AR.eq_int_int(c, 110) then
      if p1 + 4 <= max then
        (if at4(src, p1, max, 110, 117, 108, 108) then $R.ok(@(json_null(), p1 + 4)) else $R.err(p1))
      else $R.err(p1)
    (* true *)
    else if $AR.eq_int_int(c, 116) then
      if p1 + 4 <= max then
        (if at4(src, p1, max, 116, 114, 117, 101) then $R.ok(@(json_bool(true), p1 + 4)) else $R.err(p1))
      else $R.err(p1)
    (* false *)
    else if $AR.eq_int_int(c, 102) then
      if p1 + 5 <= max then
        (if at4(src, p1 + 1, max, 97, 108, 115, 101) then $R.ok(@(json_bool(false), p1 + 5)) else $R.err(p1))
      else $R.err(p1)
    (* string *)
    else if $AR.eq_int_int(c, 34) then let
      val @(arr, len, ep, closed) = parse_string(src, p1 + 1, max)
    in
      if closed then $R.ok(@(json_str(arr, len), ep))
      else let val () = $A.free<byte>(arr) in $R.err(ep) end
    end
    (* array *)
    else if $AR.eq_int_int(c, 91) then
      (case+ _parse_array{l}{n}{q1+1}(src, p1 + 1, max, json_list_nil(), true) of
       | ~$R.ok(@(lst, ep)) => $R.ok(@(json_arr(reverse_list(lst, json_list_nil())), ep))
       | ~$R.err(e) => $R.err(e))
    (* object *)
    else if $AR.eq_int_int(c, 123) then
      (case+ _parse_object{l}{n}{q1+1}(src, p1 + 1, max, json_entries_nil(), true) of
       | ~$R.ok(@(ents, ep)) => $R.ok(@(json_obj(reverse_entries(ents, json_entries_nil())), ep))
       | ~$R.err(e) => $R.err(e))
    (* number: the first digit is consumed here, so the end is past p1 *)
    else if c >= 48 && c <= 57 then let
      val @(fits, v, ep) = parse_int(src, p1 + 1, max, 48 - c, false)
    in if fits then $R.ok(@(json_int(v), ep)) else $R.err(p1) end
    (* negative number *)
    else if $AR.eq_int_int(c, 45) then let
      val @(fits, v, ep) = parse_int(src, p1 + 1, max, 0, true)
    in if fits then $R.ok(@(json_int(v), ep)) else $R.err(p1) end
    else $R.err(p1)
  end
end

(* The elements from pos to the closing ], onto acc (newest first) *)
and _parse_array {l:agz}{n:pos}{s:nat}{p:nat | s <= p; p <= n} .<n - p, 1>.
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n,
   acc: json_list_v, first: bool): $R.result(@(json_list_v, [q:int | s <= q; q <= n] int q), int) = let
  val p1 = skip_ws(src, pos, max)
in
  if p1 >= max then let
    val () = json_list_free(acc)
  in $R.err(p1) end
  else if $AR.eq_int_int(rd(src, p1, max), 93) then (* ] *)
    $R.ok(@(acc, p1 + 1))
  else let
    val p2 = skip_comma(src, p1, max, first)
  in
    case+ _parse(src, p2, max) of
    | ~$R.ok(@(v, ep)) => _parse_array{l}{n}{s}(src, ep, max, json_list_cons(v, acc), false)
    | ~$R.err(e) => let
        val () = json_list_free(acc)
      in $R.err(e) end
  end
end

(* The entries from pos to the closing }, onto acc (newest first) *)
and _parse_object {l:agz}{n:pos}{s:nat}{p:nat | s <= p; p <= n} .<n - p, 1>.
  (src: !$A.borrow(byte, l, n), pos: int p, max: int n,
   acc: json_entries_v, first: bool): $R.result(@(json_entries_v, [q:int | s <= q; q <= n] int q), int) = let
  val p1 = skip_ws(src, pos, max)
in
  if p1 >= max then let
    val () = json_entries_free(acc)
  in $R.err(p1) end
  else if $AR.eq_int_int(rd(src, p1, max), 125) then (* } *)
    $R.ok(@(acc, p1 + 1))
  else let
    val p2 = skip_comma(src, p1, max, first)
  in
    if p2 >= max then let
      val () = json_entries_free(acc)
    in $R.err(p2) end
    else if $AR.eq_int_int(rd(src, p2, max), 34) then let (* " for key *)
      val @(karr, klen, kep, kclosed) = parse_string(src, p2 + 1, max)
      val p3 = skip_ws(src, kep, max)
    in
      if ~kclosed then let
        val () = $A.free<byte>(karr)
        val () = json_entries_free(acc)
      in $R.err(kep) end
      else if p3 >= max then let
        val () = $A.free<byte>(karr)
        val () = json_entries_free(acc)
      in $R.err(p3) end
      else if $AR.eq_int_int(rd(src, p3, max), 58) then (* : *)
        (case+ _parse(src, p3 + 1, max) of
         | ~$R.ok(@(v, vep)) =>
             _parse_object{l}{n}{s}(src, vep, max, json_entries_cons(karr, klen, v, acc), false)
         | ~$R.err(e) => let
             val () = $A.free<byte>(karr)
             val () = json_entries_free(acc)
           in $R.err(e) end)
      else let
        val () = $A.free<byte>(karr)
        val () = json_entries_free(acc)
      in $R.err(p3) end
    end
    else let
      val () = json_entries_free(acc)
    in $R.err(p2) end
  end
end

implement parse (src, pos, max) =
  case+ _parse(src, pos, max) of
  | ~$R.ok(@(v, q)) => $R.ok(@(v, q))
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

(* The kind of the value s parses to (0 null, 1 bool, 2 int, 3 string,
   4 array, 5 object; ~1 when it does not parse to its end), and the
   int or the string's length *)
fn parse_kind {sn:pos | sn <= 256} (s: string sn): @(int, int) = let
  val b = $B.create()
  val () = $B.bput(b, s)
  val @(arr, n) = $B.to_arr(b)
  val @(f, bv) = $A.freeze<byte>(arr)
  val r = (case+ parse(bv, 0, 524288) of
    | ~$R.ok(@(v, ep)) => let
        val kv = (case+ v of
          | json_null() => @(0, 0)
          | json_bool(_) => @(1, 0)
          | json_int(i) => @(2, i)
          | json_str(_, len) => @(3, len)
          | json_arr(_) => @(4, 0)
          | json_obj(_) => @(5, 0)): @(int, int)
        val () = json_free(v)
      in if ep = n then kv else @(~1, 0) end
    | ~$R.err(_) => @(~1, 0)): @(int, int)
  val () = $A.drop<byte>(f, bv)
  val () = $A.free<byte>($A.thaw<byte>(f))
in r end

fn test_serialize_null (): bool = serializes_to(json_null(), "null")

fn test_serialize_true (): bool = serializes_to(json_bool(true), "true")

fn test_serialize_int (): bool = serializes_to(json_int(42), "42")

fn test_serialize_string (): bool = let
  val s = $A.alloc<byte>(4096)
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

end
