#include "share/atspre_staload.hats"
#use array as A
#use builder as B
#use json as J
#use result as R

(* The caps, at and around them: strings (values and keys) past the old
   4096-byte buffer, and just under, at and just over STRING_CAP
   (1048576 bytes decoded), also when an escape makes the text longer
   than the string; number lexemes at and over the same cap; nesting at and
   over DEPTH_CAP (512). Inputs this big do not fit in one $A.alloc, so they
   are pieces of an arena. Then round trips through serialize_rope at
   the cap, and through serialize for a string past 4096 bytes.
   One line per check; exits 1 on any failure. *)

macdef CAP = 1048576
macdef DEPTH = 512

fn report (name: string, ok: bool): bool = let
  val () = (if ok then println! ("ok   ", name) else println! ("FAIL ", name))
in ok end

(* Error codes: the parse_error's kind, negated, and its offset *)
fn err_code (e: $J.parse_error): @(int, int) =
  case+ e of
  | ~$J.UnexpectedEnd(p) => @(~1, p)
  | ~$J.UnexpectedByte(p) => @(~2, p)
  | ~$J.BadNumber(p) => @(~3, p)
  | ~$J.NumberTooLong(p) => @(~4, p)
  | ~$J.StringTooLong(p) => @(~5, p)
  | ~$J.ControlInString(p) => @(~6, p)
  | ~$J.BadEscape(p) => @(~7, p)
  | ~$J.BadHex(p) => @(~8, p)
  | ~$J.InvalidUtf8(p) => @(~9, p)
  | ~$J.TooDeep(p) => @(~10, p)
  | ~$J.TrailingData(p) => @(~11, p)

(* v at [i, j) (up to the end of a) *)
fun fill {l:agz}{o:addr}{n:nat}{i:nat | i <= n}{v:nat | v < 256} .<n - i>.
  (a: !$A.arrx(byte, l, n, o), n: int n, i: int i, j: int, v: int v): void =
  if i >= n then ()
  else if i >= j then ()
  else let val () = $A.write_byte(a, i, v) in fill(a, n, i + 1, j, v) end

(* r copies of the six bytes \u00XX of x at p (up to the end of a) *)
fun fill_u {l:agz}{o:addr}{n:nat}{p:nat | p <= n}{x1,x2:nat | x1 < 256; x2 < 256} .<n - p>.
  (a: !$A.arrx(byte, l, n, o), n: int n, p: int p, r: int, x1: int x1, x2: int x2): void =
  if r <= 0 then ()
  else if p + 6 <= n then let
    val () = $A.write_byte(a, p, 92)
    val () = $A.write_byte(a, p + 1, 117)
    val () = $A.write_byte(a, p + 2, 48)
    val () = $A.write_byte(a, p + 3, 48)
    val () = $A.write_byte(a, p + 4, x1)
    val () = $A.write_byte(a, p + 5, x2)
  in fill_u(a, n, p + 6, r - 1, x1, x2) end
  else ()

(* r copies of the five bytes {"a": at p (up to the end of a) *)
fun fill_open {l:agz}{o:addr}{n:nat}{p:nat | p <= n} .<n - p>.
  (a: !$A.arrx(byte, l, n, o), n: int n, p: int p, r: int): void =
  if r <= 0 then ()
  else if p + 5 <= n then let
    val () = $A.write_byte(a, p, 123)
    val () = $A.write_byte(a, p + 1, 34)
    val () = $A.write_byte(a, p + 2, 97)
    val () = $A.write_byte(a, p + 3, 34)
    val () = $A.write_byte(a, p + 4, 58)
  in fill_open(a, n, p + 5, r - 1) end
  else ()

(* Whether a[0, k) is all v, but for its last two bytes, C3 A9 (U+00E9),
   when w >= 0 *)
fun all_is {l:agz}{c:pos}{k:nat | k <= c}{i:nat | i <= k} .<k - i>.
  (a: !$A.arr(byte, l, c), i: int i, k: int k, v: int, w: int): bool =
  if i >= k then true
  else let
    val want = (if w >= 0 && i = k - 2 then 195 else if w >= 0 && i = k - 1 then 169 else v): int
  in
    if byte2int0($A.get<byte>(a, i)) = want then all_is(a, i + 1, k, v, w) else false
  end

(* Whether a[0, k) and b[0, k) are the same *)
fun same {la,lb:agz}{ca,cb:pos}{k:nat | k <= ca; k <= cb}{i:nat | i <= k} .<k - i>.
  (a: !$A.arr(byte, la, ca), b: !$A.arr(byte, lb, cb), i: int i, k: int k): bool =
  if i >= k then true
  else if byte2int0($A.get<byte>(a, i)) = byte2int0($A.get<byte>(b, i)) then same(a, b, i + 1, k)
  else false

(* Whether v and w are the same string, or both arrays *)
fn same_str {sv,sw:nat} (v: !$J.json(sv), w: !$J.json(sw)): bool =
  case+ v of
  | $J.json_str(a, an) =>
      (case+ w of
       | $J.json_str(b, bn) => if an = bn then same(a, b, 0, an) else false
       | _ => false)
  | $J.json_arr(_) => (case+ w of $J.json_arr(_) => true | _ => false)
  | _ => false

(* What a whole text parses to: 4 a string, 3 a number, 5 an array,
   6 an object, or the error's code; with the string's length, the
   lexeme's length or the error's offset; and whether the string's or
   lexeme's bytes are all v (ending in U+00E9 when w >= 0) *)
fn outcome (r: $R.result($J.json_v, $J.parse_error), v: int, w: int): @(int, int, bool) =
  case+ r of
  | ~$R.ok(x) => let
      val o = (case+ x of
        | $J.json_str(a, n) => @(4, n, all_is(a, 0, n, v, w))
        | $J.json_num(a, n, $R.none()) => @(3, n, all_is(a, 0, n, v, w))
        | $J.json_num(_, _, $R.some(_)) => @(~99, 0, false)
        | $J.json_arr(_) => @(5, 0, true)
        | $J.json_obj($J.json_entries_cons(k, n, _, _)) => @(6, n, all_is(k, 0, n, v, w))
        | _ => @(~99, 0, false)): @(int, int, bool)
      val () = $J.json_free(x)
    in o end
  | ~$R.err(e) => let val @(c, p) = err_code(e) in @(c, p, true) end

(* The kinds of text, of n bytes, the cases build: k is a length or a
   count *)
macdef STR = 0      (* "a...a": k a's *)
macdef KEY = 1      (* {"a...a":1}: k a's *)
macdef ESC = 2      (* "a...a\u00e9": k a's, then an escape of 2 bytes decoded *)
macdef ESCN = 6     (* "a...a\n": k a's, then an escape of 1 byte decoded *)
macdef RAW2 = 7     (* "a...a" then raw C3 A9: k a's, then 2 raw bytes *)
macdef PAIR = 8     (* "a...a\ud83d\ude00": k a's, then a pair of 4 bytes decoded *)
macdef NUM = 3      (* 1...1: k digits *)
macdef ARRS = 4     (* [[...]]: k levels *)
macdef OBJS = 5     (* {"a":{"a":...0}}: k levels *)

(* Fills a[0, n) with the text of kind t for k; false when n is not
   its length *)
fn build {l:agz}{o:addr}{n:pos}
  (a: !$A.arrx(byte, l, n, o), n: int n, t: int, k: int): bool =
  if n < 16 then false
  else if t = STR then
    (if k + 2 = n then let
       val () = $A.write_byte(a, 0, 34)
       val () = fill(a, n, 1, n - 1, 97)
       val () = $A.write_byte(a, n - 1, 34)
     in true end
     else false)
  else if t = KEY then
    (if k + 6 = n then let
       val () = $A.write_byte(a, 0, 123)
       val () = $A.write_byte(a, 1, 34)
       val () = fill(a, n, 2, n - 4, 97)
       val () = $A.write_byte(a, n - 4, 34)
       val () = $A.write_byte(a, n - 3, 58)
       val () = $A.write_byte(a, n - 2, 49)
       val () = $A.write_byte(a, n - 1, 125)
     in true end
     else false)
  else if t = ESC then
    (if k + 8 = n then let
       val () = $A.write_byte(a, 0, 34)
       val () = fill(a, n, 1, n - 7, 97)
       val () = fill_u(a, n, n - 7, 1, 101, 57)
       val () = $A.write_byte(a, n - 1, 34)
     in true end
     else false)
  else if t = ESCN then
    (if k + 4 = n then let
       val () = $A.write_byte(a, 0, 34)
       val () = fill(a, n, 1, n - 3, 97)
       val () = $A.write_byte(a, n - 3, 92)
       val () = $A.write_byte(a, n - 2, 110)
       val () = $A.write_byte(a, n - 1, 34)
     in true end
     else false)
  else if t = RAW2 then
    (if k + 4 = n then let
       val () = $A.write_byte(a, 0, 34)
       val () = fill(a, n, 1, n - 3, 97)
       val () = $A.write_byte(a, n - 3, 195)
       val () = $A.write_byte(a, n - 2, 169)
       val () = $A.write_byte(a, n - 1, 34)
     in true end
     else false)
  else if t = PAIR then
    (if k + 14 = n then let
       val () = $A.write_byte(a, 0, 34)
       val () = fill(a, n, 1, n - 13, 97)
       val () = fill_u(a, n, n - 13, 1, 0, 0)
       val () = fill_u(a, n, n - 7, 1, 0, 0)
       (* \u00XX twice, made \ud83d\ude00 *)
       val () = $A.write_byte(a, n - 11, 100)
       val () = $A.write_byte(a, n - 10, 56)
       val () = $A.write_byte(a, n - 9, 51)
       val () = $A.write_byte(a, n - 8, 100)
       val () = $A.write_byte(a, n - 5, 100)
       val () = $A.write_byte(a, n - 4, 101)
       val () = $A.write_byte(a, n - 3, 48)
       val () = $A.write_byte(a, n - 2, 48)
       val () = $A.write_byte(a, n - 1, 34)
     in true end
     else false)
  else if t = NUM then
    (if k = n then let val () = fill(a, n, 0, n, 49) in true end else false)
  else if t = ARRS then
    (if 2 * k = n then let
       val () = fill(a, n, 0, k, 91)
       fun close {i:nat | i <= n} .<n - i>. (a: !$A.arrx(byte, l, n, o), n: int n, i: int i, k: int): void =
         if i >= n then ()
         else let
           val () = (if i >= k then $A.write_byte(a, i, 93) else ())
         in close(a, n, i + 1, k) end
       val () = close(a, n, 0, k)
     in true end
     else false)
  else if t = OBJS then
    (if 6 * k + 1 = n then let
       val () = fill_open(a, n, 0, k)
       fun close {i:nat | i <= n} .<n - i>. (a: !$A.arrx(byte, l, n, o), n: int n, i: int i, k: int): void =
         if i >= n then ()
         else let
           val () = (if i = 5 * k then $A.write_byte(a, i, 48)
                     else if i > 5 * k then $A.write_byte(a, i, 125) else ())
         in close(a, n, i + 1, k) end
       val () = close(a, n, 0, k)
     in true end
     else false)
  else false

(* Parses the text of kind t for k, n bytes, held in an arena piece *)
fn parse_case {n:pos | n <= 268435456} (n: int n, t: int, k: int, v: int, w: int): @(int, int, bool) =
  case+ $A.arena_create<byte>(n) of
  | ~$A.arena_some(ar) => let
      val a = $A.arena_alloc<byte>(ar, n)
      val built = build(a, n, t, k)
      val @(f, bv) = $A.freeze<byte>(a)
      val r = $J.parse_text(bv, n)
      val () = $A.drop<byte>(f, bv)
      val () = $A.arena_return<byte>(ar, $A.thaw<byte>(f))
      val () = $A.arena_destroy<byte>(ar)
      val o = outcome(r, v, w)
    in if built then o else @(~98, 0, false) end
  | ~$A.arena_none() => @(~97, 0, false)

(* The total length of a rope's chunks *)
fun rope_len {k:nat} .<k>. (cs: !$B.rope_list(k)): [t:nat] int t =
  case+ cs of
  | $B.rope_nil() => 0
  | $B.rope_cons(_, n, tl) => n + rope_len(tl)

(* The chunks into a from off on; false when one does not fit *)
fun rope_into {l:agz}{o:addr}{m:nat}{k:nat}{f:nat} .<k>.
  (cs: !$B.rope_list(k), a: !$A.arrx(byte, l, m, o), m: int m, off: int f): bool =
  case+ cs of
  | $B.rope_nil() => true
  | $B.rope_cons(c, n, tl) => let
      fun copy {lc:agz}{n:nat | n <= $B.BUILDER_CAP}{i:nat | i <= n} .<n - i>.
        (c: !$A.arr(byte, lc, $B.BUILDER_CAP), a: !$A.arrx(byte, l, m, o), m: int m,
         off: int f, i: int i, n: int n): bool =
        if i >= n then true
        else let
          val at = off + i
        in
          if at < m then let
            val () = $A.set<byte>(a, at, $A.get<byte>(c, i))
          in copy(c, a, m, off, i + 1, n) end
          else false
        end
    in
      if copy(c, a, m, off, 0, n) then rope_into(tl, a, m, off + n) else false
    end

(* v through serialize_rope and parse_text: whether it comes back the
   same string (or an array, for an array), and the serialized length *)
fn rope_roundtrip {sz:nat} (v: $J.json(sz)): @(bool, int) = let
  val r = $B.rope_create()
  val () = $J.serialize_rope(v, r)
  val cs = $B.rope_chunks(r)
  val total = rope_len(cs)
in
  if total <= 0 then let
    val () = $B.rope_list_free(cs)
    val () = $J.json_free(v)
  in @(false, total) end
  else if total > 268435456 then let
    val () = $B.rope_list_free(cs)
    val () = $J.json_free(v)
  in @(false, total) end
  else let
    val m = total
  in
    case+ $A.arena_create<byte>(m) of
    | ~$A.arena_some(ar) => let
        val a = $A.arena_alloc<byte>(ar, m)
        val copied = rope_into(cs, a, m, 0)
        val () = $B.rope_list_free(cs)
        val @(f, bv) = $A.freeze<byte>(a)
        val back = $J.parse_text(bv, m)
        val () = $A.drop<byte>(f, bv)
        val () = $A.arena_return<byte>(ar, $A.thaw<byte>(f))
        val () = $A.arena_destroy<byte>(ar)
        val ok = (case+ back of
          | ~$R.ok(w) => let
              val s = same_str(v, w)
              val () = $J.json_free(w)
            in s end
          | ~$R.err(e) => let val _ = err_code(e) in false end): bool
        val () = $J.json_free(v)
      in @(copied && ok, total) end
    | ~$A.arena_none() => let
        val () = $B.rope_list_free(cs)
        val () = $J.json_free(v)
      in @(false, 0) end
  end
end

(* The string the text of kind t for k parses to, through rope_roundtrip *)
fn parse_roundtrip {n:pos | n <= 268435456} (n: int n, t: int, k: int): @(bool, int) =
  case+ $A.arena_create<byte>(n) of
  | ~$A.arena_some(ar) => let
      val a = $A.arena_alloc<byte>(ar, n)
      val built = build(a, n, t, k)
      val @(f, bv) = $A.freeze<byte>(a)
      val r = $J.parse_text(bv, n)
      val () = $A.drop<byte>(f, bv)
      val () = $A.arena_return<byte>(ar, $A.thaw<byte>(f))
      val () = $A.arena_destroy<byte>(ar)
    in
      case+ r of
      | ~$R.ok(v) => let
          val @(ok, len) = rope_roundtrip(v)
        in @(built && ok, len) end
      | ~$R.err(e) => let val _ = err_code(e) in @(false, 0) end
    end
  | ~$A.arena_none() => @(false, 0)

(* A string of k bytes, all v *)
fn str_k {k:pos | k <= 1048576}{v:nat | v < 256} (k: int k, v: int v): $J.json(6 * k + 2) = let
  val a = $A.alloc<byte>(k)
  val () = fill(a, k, 0, k, v)
in $J.json_str(a, k) end

implement main0 () = let
  val s1 = report("string of 5000 bytes", let
      val @(c, x, ok) = parse_case(5002, STR, 5000, 97, ~1) in c = 4 && x = 5000 && ok end)
  val s2 = report("string one under the cap", let
      val @(c, x, ok) = parse_case(CAP + 1, STR, CAP - 1, 97, ~1) in c = 4 && x = CAP - 1 && ok end)
  val s3 = report("string at the cap", let
      val @(c, x, ok) = parse_case(CAP + 2, STR, CAP, 97, ~1) in c = 4 && x = CAP && ok end)
  val s4 = report("string one over the cap: StringTooLong at its quote", let
      val @(c, x, _) = parse_case(CAP + 3, STR, CAP + 1, 97, ~1) in c = ~5 && x = 0 end)
  val k1 = report("key of 5000 bytes", let
      val @(c, x, ok) = parse_case(5006, KEY, 5000, 97, ~1) in c = 6 && x = 5000 && ok end)
  val k2 = report("key at the cap", let
      val @(c, x, ok) = parse_case(CAP + 6, KEY, CAP, 97, ~1) in c = 6 && x = CAP && ok end)
  val k3 = report("key one over the cap: StringTooLong at its quote", let
      val @(c, x, _) = parse_case(CAP + 7, KEY, CAP + 1, 97, ~1) in c = ~5 && x = 1 end)
  val x1 = report("string reaching the cap with its last escape (text longer than the cap)", let
      val @(c, x, ok) = parse_case(CAP + 6, ESC, CAP - 2, 97, 0)
    in c = 4 && x = CAP && ok end)
  val x2 = report("string whose last escape crosses the cap", let
      val @(c, x, _) = parse_case(CAP + 7, ESC, CAP - 1, 97, ~1)
    in c = ~5 && x = 0 end)
  val x3 = report("string reaching the cap with \\n", let
      val @(c, x, _) = parse_case(CAP + 3, ESCN, CAP - 1, 97, ~1) in c = 4 && x = CAP end)
  val x4 = report("string whose \\n crosses the cap", let
      val @(c, x, _) = parse_case(CAP + 4, ESCN, CAP, 97, ~1) in c = ~5 && x = 0 end)
  val x5 = report("string reaching the cap with raw two-byte UTF-8", let
      val @(c, x, ok) = parse_case(CAP + 2, RAW2, CAP - 2, 97, 0) in c = 4 && x = CAP && ok end)
  val x6 = report("string whose raw two-byte UTF-8 crosses the cap", let
      val @(c, x, _) = parse_case(CAP + 3, RAW2, CAP - 1, 97, ~1) in c = ~5 && x = 0 end)
  val x7 = report("string reaching the cap with a surrogate pair", let
      val @(c, x, _) = parse_case(CAP + 10, PAIR, CAP - 4, 97, ~1) in c = 4 && x = CAP end)
  val x8 = report("string whose surrogate pair crosses the cap", let
      val @(c, x, _) = parse_case(CAP + 11, PAIR, CAP - 3, 97, ~1) in c = ~5 && x = 0 end)
  val m1 = report("number lexeme at the cap", let
      val @(c, x, ok) = parse_case(CAP, NUM, CAP, 49, ~1) in c = 3 && x = CAP && ok end)
  val m2 = report("number lexeme one over the cap: NumberTooLong", let
      val @(c, x, _) = parse_case(CAP + 1, NUM, CAP + 1, 49, ~1) in c = ~4 && x = 0 end)
  val d1 = report("arrays nested DEPTH_CAP deep", let
      val @(c, _, _) = parse_case(2 * DEPTH, ARRS, DEPTH, 0, ~1) in c = 5 end)
  val d2 = report("arrays nested one deeper: TooDeep at the bracket", let
      val @(c, x, _) = parse_case(2 * (DEPTH + 1), ARRS, DEPTH + 1, 0, ~1) in c = ~10 && x = DEPTH end)
  val d3 = report("objects nested DEPTH_CAP deep", let
      val @(c, _, _) = parse_case(6 * DEPTH + 1, OBJS, DEPTH, 97, ~1) in c = 6 end)
  val d4 = report("objects nested one deeper: TooDeep at the brace", let
      val @(c, x, _) = parse_case(6 * (DEPTH + 1) + 1, OBJS, DEPTH + 1, 97, ~1)
    in c = ~10 && x = 5 * DEPTH end)
  val r1 = report("round trip of a string at the cap", let
      val @(ok, len) = parse_roundtrip(CAP + 2, STR, CAP) in ok && len = CAP + 2 end)
  val r2 = report("round trip of an escaped string at the cap (written as raw UTF-8)", let
      val @(ok, len) = parse_roundtrip(CAP + 6, ESC, CAP - 2) in ok && len = CAP + 2 end)
  val r3 = report("round trip of 200000 control bytes (written as \\u0001)", let
      val @(ok, len) = rope_roundtrip(str_k(200000, 1)) in ok && len = 6 * 200000 + 2 end)
  val r4 = report("serialize a string of 10000 bytes to a builder", let
      val v = str_k(10000, 34)
      val b = $B.create()
      val () = $J.serialize(v, b)
      val n = $B.length(b)
      val () = $B.builder_free(b)
      val @(ok, len) = rope_roundtrip(v)
    in ok && n = 20002 && len = 20002 end)
  val r5 = report("round trip of arrays nested DEPTH_CAP deep", let
      val @(ok, len) = parse_roundtrip(2 * DEPTH, ARRS, DEPTH) in ok && len = 2 * DEPTH end)
in
  if s1 && s2 && s3 && s4 && k1 && k2 && k3 && x1 && x2 && x3 && x4 && x5 && x6 && x7 && x8 && m1 && m2 && d1 && d2 && d3 && d4 &&
     r1 && r2 && r3 && r4 && r5
  then println! ("caps: all cases pass")
  else exit_void(1)
end
