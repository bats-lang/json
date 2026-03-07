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

implement json_free(v) =
  case+ v of
  | ~json_null() => ()
  | ~json_bool(_) => ()
  | ~json_int(_) => ()
  | ~json_str(arr, _) => $A.free<byte>(arr)
  | ~json_arr(lst) => json_list_free(lst)
  | ~json_obj(ents) => json_entries_free(ents)

implement json_list_free(lst) =
  case+ lst of
  | ~json_list_nil() => ()
  | ~json_list_cons(v, rest) => let
      val () = json_free(v)
    in json_list_free(rest) end

implement json_entries_free(ents) =
  case+ ents of
  | ~json_entries_nil() => ()
  | ~json_entries_cons(k, _, v, rest) => let
      val () = $A.free<byte>(k)
      val () = json_free(v)
    in json_entries_free(rest) end

(* ============================================================
   Serialize: JSON value → builder (compile-time bounds only)
   ============================================================ *)

(* Helper: write a byte array to builder, escaping for JSON strings.
   Each input byte produces at most 2 output bytes (escape sequences).
   With bounded iteration via pos < len <= 4096, max output is 8192. *)
fn emit_escaped {l:agz}{n:nat | n + 8192 <= $B.BUILDER_CAP}
    {dlen:nat | dlen <= 4096}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + 8192] $B.builder(m),
   arr: !$A.arr(byte, l, 4096), len: int dlen): void = let
  fun loop {l2:agz}{pos:nat | pos <= dlen}{room:nat | room >= 2; p:nat | p + room <= $B.BUILDER_CAP} .<dlen - pos>.
    (b: !$B.builder(p) >> [m:nat | p <= m; m <= p + room] $B.builder(m),
     arr: !$A.arr(byte, l2, 4096), pos: int pos, len: int dlen, room: int room): void =
    if pos >= len then ()
    else if room < 4 then ()
    else let
      val c = byte2int0($A.get<byte>(arr, pos))
    in
      if $AR.eq_int_int(c, 34) then let (* " *)
        val () = $B.put_byte(b, 92) val () = $B.put_byte(b, 34)
      in loop(b, arr, pos + 1, len, room - 2) end
      else if $AR.eq_int_int(c, 92) then let (* \ *)
        val () = $B.put_byte(b, 92) val () = $B.put_byte(b, 92)
      in loop(b, arr, pos + 1, len, room - 2) end
      else if $AR.eq_int_int(c, 10) then let (* \n *)
        val () = $B.put_byte(b, 92) val () = $B.put_byte(b, 110)
      in loop(b, arr, pos + 1, len, room - 2) end
      else if $AR.eq_int_int(c, 9) then let (* \t *)
        val () = $B.put_byte(b, 92) val () = $B.put_byte(b, 116)
      in loop(b, arr, pos + 1, len, room - 2) end
      else if $AR.eq_int_int(c, 13) then let (* \r *)
        val () = $B.put_byte(b, 92) val () = $B.put_byte(b, 114)
      in loop(b, arr, pos + 1, len, room - 2) end
      else let
        val () = $B.put_byte(b, c)
      in loop(b, arr, pos + 1, len, room - 2) end
    end
in loop(b, arr, 0, len, 8192) end

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

implement serialize(v, b) =
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
      val () = serialize_list(lst, b, true)
    in $B.put_byte(b, 93) end
  | json_obj(ents) => let
      val () = $B.put_byte(b, 123)
      val () = serialize_entries(ents, b, true)
    in $B.put_byte(b, 125) end

implement serialize_list(lst, b, first) =
  case+ lst of
  | json_list_nil() => ()
  | json_list_cons(v, rest) => let
      val () = (if ~first then $B.put_byte(b, 44) else ())
      val () = serialize(v, b)
    in serialize_list(rest, b, false) end

implement serialize_entries(ents, b, first) =
  case+ ents of
  | json_entries_nil() => ()
  | json_entries_cons(k, klen, v, rest) => let
      val () = (if ~first then $B.put_byte(b, 44) else ())
      val () = $B.put_byte(b, 34)
      val () = emit_escaped(b, k, klen)
      val () = $B.put_byte(b, 34)
      val () = $B.put_byte(b, 58)
      val () = serialize(v, b)
    in serialize_entries(rest, b, false) end

(* ============================================================
   Deserialize: byte buffer → JSON value
   ============================================================ *)

fn rd {l:agz}{n:pos}
  (src: !$A.borrow(byte, l, n), pos: int, max: int n): int =
  let val p = $AR.checked_idx(pos, max) in
    if p >= 0 then byte2int0($A.read<byte>(src, p))
    else 0
  end

fun skip_ws {l:agz}{n:pos}{fuel:nat} .<fuel>.
  (src: !$A.borrow(byte, l, n), pos: int, max: int n, fuel: int fuel): int =
  if fuel <= 0 then pos
  else let val c = rd(src, pos, max) in
    if $AR.eq_int_int(c, 32) || $AR.eq_int_int(c, 9) ||
       $AR.eq_int_int(c, 10) || $AR.eq_int_int(c, 13)
    then skip_ws(src, pos + 1, max, fuel - 1)
    else pos
  end

fun parse_string {l:agz}{n:pos}{fuel:nat} .<fuel>.
  (src: !$A.borrow(byte, l, n), pos: int, max: int n, fuel: int fuel
  ): @([ls:agz] $A.arr(byte, ls, 4096), [dlen:nat | dlen <= 4096] int dlen, int) =
  let
    val out = $A.alloc<byte>(4096)
    fun loop {lo:agz}{opos:nat | opos <= 4096}{fuel2:nat} .<fuel2>.
      (src: !$A.borrow(byte, l, n), pos: int, max: int n,
       out: !$A.arr(byte, lo, 4096), opos: int opos, fuel2: int fuel2): @([r:nat | r <= 4096] int r, int) =
      if fuel2 <= 0 then @(opos, pos)
      else let val c = rd(src, pos, max) in
        if $AR.eq_int_int(c, 34) then @(opos, pos + 1) (* closing " *)
        else if opos >= 4095 then @(opos, pos) (* buffer full *)
        else if $AR.eq_int_int(c, 92) then let (* backslash escape *)
          val c2 = rd(src, pos + 1, max)
          val ec = (if $AR.eq_int_int(c2, 110) then 10        (* \n *)
                    else if $AR.eq_int_int(c2, 116) then 9    (* \t *)
                    else if $AR.eq_int_int(c2, 114) then 13   (* \r *)
                    else if $AR.eq_int_int(c2, 34) then 34    (* \" *)
                    else if $AR.eq_int_int(c2, 92) then 92    (* \\ *)
                    else c2): int
          val () = $A.set<byte>(out, opos, int2byte0(ec))
        in loop(src, pos + 2, max, out, opos + 1, fuel2 - 1) end
        else let
          val () = $A.set<byte>(out, opos, int2byte0(c))
        in loop(src, pos + 1, max, out, opos + 1, fuel2 - 1) end
      end
    val @(olen, epos) = loop(src, pos, max, out, 0, fuel)
  in @(out, olen, epos) end

fun parse_int {l:agz}{n:pos}{fuel:nat} .<fuel>.
  (src: !$A.borrow(byte, l, n), pos: int, max: int n,
   acc: int, neg: bool, fuel: int fuel): @(int, int) =
  if fuel <= 0 then @((if neg then ~acc else acc), pos)
  else let val c = rd(src, pos, max) in
    if c >= 48 then if c <= 57 then
      parse_int(src, pos + 1, max, acc * 10 + (c - 48), neg, fuel - 1)
    else @((if neg then ~acc else acc), pos)
    else @((if neg then ~acc else acc), pos)
  end

#pub fun parse {l:agz}{n:pos}
  (src: !$A.borrow(byte, l, n), pos: int, max: int n
  ): $R.result(@(json_v, int), int)

fn skip_comma {l:agz}{n:pos}
  (src: !$A.borrow(byte, l, n), pos: int, max: int n, first: bool): int =
  if first then pos
  else let
    val pc = skip_ws(src, pos, max, 256)
    val cc = rd(src, pc, max)
  in
    if $AR.eq_int_int(cc, 44) then skip_ws(src, pc + 1, max, 256)
    else pc
  end

fun parse_array {l:agz}{n:pos}{fuel:nat} .<fuel>.
  (src: !$A.borrow(byte, l, n), pos: int, max: int n,
   acc: json_list_v, first: bool, fuel: int fuel): $R.result(@(json_list_v, int), int) =
  if fuel <= 0 then let
    val () = json_list_free(acc)
  in $R.err(pos) end
  else let
    val p = skip_ws(src, pos, max, 256)
    val c = rd(src, p, max)
  in
    if $AR.eq_int_int(c, 93) then (* ] *)
      $R.ok(@(acc, p + 1))
    else let
      val p2 = skip_comma(src, p, max, first)
      val vr = parse(src, p2, max)
    in
      case+ vr of
      | ~$R.ok(@(v, ep)) =>
          parse_array(src, ep, max, json_list_cons(v, acc), false, fuel - 1)
      | ~$R.err(e) => let
          val () = json_list_free(acc)
        in $R.err(e) end
    end
  end

fun parse_object {l:agz}{n:pos}{fuel:nat} .<fuel>.
  (src: !$A.borrow(byte, l, n), pos: int, max: int n,
   acc: json_entries_v, first: bool, fuel: int fuel): $R.result(@(json_entries_v, int), int) =
  if fuel <= 0 then let
    val () = json_entries_free(acc)
  in $R.err(pos) end
  else let
    val p = skip_ws(src, pos, max, 256)
    val c = rd(src, p, max)
  in
    if $AR.eq_int_int(c, 125) then (* } *)
      $R.ok(@(acc, p + 1))
    else let
      val p2 = skip_comma(src, p, max, first)
      val ck = rd(src, p2, max)
    in
      if $AR.eq_int_int(ck, 34) then let (* " for key *)
        val @(karr, klen, kep) = parse_string(src, p2 + 1, max, $AR.checked_nat(4096))
        val p3 = skip_ws(src, kep, max, 256)
        val colon = rd(src, p3, max)
      in
        if $AR.eq_int_int(colon, 58) then let (* : *)
          val p4 = skip_ws(src, p3 + 1, max, 256)
          val vr = parse(src, p4, max)
        in
          case+ vr of
          | ~$R.ok(@(v, vep)) =>
              parse_object(src, vep, max,
                json_entries_cons(karr, klen, v, acc), false, fuel - 1)
          | ~$R.err(e) => let
              val () = $A.free<byte>(karr)
              val () = json_entries_free(acc)
            in $R.err(e) end
        end
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

fun reverse_list {fuel:nat} .<fuel>.
  (lst: json_list_v, acc: json_list_v, fuel: int fuel): json_list_v =
  if fuel <= 0 then let val () = json_list_free(lst) in acc end
  else case+ lst of
  | ~json_list_nil() => acc
  | ~json_list_cons(v, rest) => reverse_list(rest, json_list_cons(v, acc), fuel - 1)

fun reverse_entries {fuel:nat} .<fuel>.
  (ents: json_entries_v, acc: json_entries_v, fuel: int fuel): json_entries_v =
  if fuel <= 0 then let val () = json_entries_free(ents) in acc end
  else case+ ents of
  | ~json_entries_nil() => acc
  | ~json_entries_cons(k, kl, v, rest) =>
      reverse_entries(rest, json_entries_cons(k, kl, v, acc), fuel - 1)

implement parse (src, pos, max) = let
  val p = skip_ws(src, pos, max, 256)
  val c = rd(src, p, max)
in
  (* null *)
  if $AR.eq_int_int(c, 110) then (* n *)
    if $AR.eq_int_int(rd(src, p+1, max), 117) then
    if $AR.eq_int_int(rd(src, p+2, max), 108) then
    if $AR.eq_int_int(rd(src, p+3, max), 108) then
      $R.ok(@(json_null(), p + 4))
    else $R.err(p)
    else $R.err(p)
    else $R.err(p)
  (* true *)
  else if $AR.eq_int_int(c, 116) then (* t *)
    if $AR.eq_int_int(rd(src, p+1, max), 114) then
    if $AR.eq_int_int(rd(src, p+2, max), 117) then
    if $AR.eq_int_int(rd(src, p+3, max), 101) then
      $R.ok(@(json_bool(true), p + 4))
    else $R.err(p)
    else $R.err(p)
    else $R.err(p)
  (* false *)
  else if $AR.eq_int_int(c, 102) then (* f *)
    if $AR.eq_int_int(rd(src, p+1, max), 97) then
    if $AR.eq_int_int(rd(src, p+2, max), 108) then
    if $AR.eq_int_int(rd(src, p+3, max), 115) then
    if $AR.eq_int_int(rd(src, p+4, max), 101) then
      $R.ok(@(json_bool(false), p + 5))
    else $R.err(p)
    else $R.err(p)
    else $R.err(p)
    else $R.err(p)
  (* string *)
  else if $AR.eq_int_int(c, 34) then let (* " *)
    val @(arr, len, ep) = parse_string(src, p + 1, max, $AR.checked_nat(4096))
  in $R.ok(@(json_str(arr, len), ep)) end
  (* array *)
  else if $AR.eq_int_int(c, 91) then let (* [ *)
    val lr = parse_array(src, p + 1, max, json_list_nil(), true, 1000)
  in
    case+ lr of
    | ~$R.ok(@(lst, ep)) =>
        $R.ok(@(json_arr(reverse_list(lst, json_list_nil(), 1000)), ep))
    | ~$R.err(e) => $R.err(e)
  end
  (* object *)
  else if $AR.eq_int_int(c, 123) then let (* { *)
    val er = parse_object(src, p + 1, max, json_entries_nil(), true, 1000)
  in
    case+ er of
    | ~$R.ok(@(ents, ep)) =>
        $R.ok(@(json_obj(reverse_entries(ents, json_entries_nil(), 1000)), ep))
    | ~$R.err(e) => $R.err(e)
  end
  (* number *)
  else if c >= 48 then if c <= 57 then let (* 0-9 *)
    val @(n, ep) = parse_int(src, p, max, 0, false, 100)
  in $R.ok(@(json_int(n), ep)) end
  else $R.err(p)
  (* negative number *)
  else if $AR.eq_int_int(c, 45) then let (* - *)
    val @(n, ep) = parse_int(src, p + 1, max, 0, true, 100)
  in $R.ok(@(json_int(n), ep)) end
  else $R.err(p)
end

(* ============================================================
   Unit tests
   ============================================================ *)

$UNITTEST.run begin

(* Test: serialize null *)
var b1 = $B.create()
val v1 = json_null()
val () = serialize(v1, b1)
val () = json_free(v1)
val @(a1, l1) = $B.to_arr(b1)
val () = $A.free<byte>(a1)

(* Test: serialize true *)
var b2 = $B.create()
val v2 = json_bool(true)
val () = serialize(v2, b2)
val () = json_free(v2)
val @(a2, l2) = $B.to_arr(b2)
val () = $A.free<byte>(a2)

(* Test: serialize integer *)
var b3 = $B.create()
val v3 = json_int(42)
val () = serialize(v3, b3)
val () = json_free(v3)
val @(a3, l3) = $B.to_arr(b3)
val () = $A.free<byte>(a3)

(* Test: serialize string *)
var b4 = $B.create()
val s4 = $A.alloc<byte>(4096)
val () = $A.write_byte(s4, 0, 104) (* h *)
val () = $A.write_byte(s4, 1, 105) (* i *)
val v4 = json_str(s4, 2)
val () = serialize(v4, b4)
val () = json_free(v4)
val @(a4, l4) = $B.to_arr(b4)
val () = $A.free<byte>(a4)

(* Test: roundtrip null *)
var rt1_b = $B.create()
val () = $B.bput(rt1_b, "null")
val @(rt1_a, rt1_l) = $B.to_arr(rt1_b)
val @(fz_rt1, bv_rt1) = $A.freeze<byte>(rt1_a)
val rt1_r = parse(bv_rt1, 0, 524288)
val () = (case+ rt1_r of
  | ~$R.ok(@(v, _)) => json_free(v)
  | ~$R.err(_) => ())
val () = $A.drop<byte>(fz_rt1, bv_rt1)
val () = $A.free<byte>($A.thaw<byte>(fz_rt1))

(* Test: roundtrip integer *)
var rt2_b = $B.create()
val () = $B.bput(rt2_b, "123")
val @(rt2_a, rt2_l) = $B.to_arr(rt2_b)
val @(fz_rt2, bv_rt2) = $A.freeze<byte>(rt2_a)
val rt2_r = parse(bv_rt2, 0, 524288)
val () = (case+ rt2_r of
  | ~$R.ok(@(v, _)) => json_free(v)
  | ~$R.err(_) => ())
val () = $A.drop<byte>(fz_rt2, bv_rt2)
val () = $A.free<byte>($A.thaw<byte>(fz_rt2))

(* Test: roundtrip string *)
var rt3_b = $B.create()
val () = $B.bput(rt3_b, "\"hello\"")
val @(rt3_a, _) = $B.to_arr(rt3_b)
val @(fz_rt3, bv_rt3) = $A.freeze<byte>(rt3_a)
val rt3_r = parse(bv_rt3, 0, 524288)
val () = (case+ rt3_r of
  | ~$R.ok(@(v, _)) => json_free(v)
  | ~$R.err(_) => ())
val () = $A.drop<byte>(fz_rt3, bv_rt3)
val () = $A.free<byte>($A.thaw<byte>(fz_rt3))

(* Test: roundtrip array *)
var rt4_b = $B.create()
val () = $B.bput(rt4_b, "[1,2,3]")
val @(rt4_a, _) = $B.to_arr(rt4_b)
val @(fz_rt4, bv_rt4) = $A.freeze<byte>(rt4_a)
val rt4_r = parse(bv_rt4, 0, 524288)
val () = (case+ rt4_r of
  | ~$R.ok(@(v, _)) => json_free(v)
  | ~$R.err(_) => ())
val () = $A.drop<byte>(fz_rt4, bv_rt4)
val () = $A.free<byte>($A.thaw<byte>(fz_rt4))

(* Test: roundtrip object *)
var rt5_b = $B.create()
val () = $B.bput(rt5_b, "{\"a\":1,\"b\":true}")
val @(rt5_a, _) = $B.to_arr(rt5_b)
val @(fz_rt5, bv_rt5) = $A.freeze<byte>(rt5_a)
val rt5_r = parse(bv_rt5, 0, 524288)
val () = (case+ rt5_r of
  | ~$R.ok(@(v, _)) => json_free(v)
  | ~$R.err(_) => ())
val () = $A.drop<byte>(fz_rt5, bv_rt5)
val () = $A.free<byte>($A.thaw<byte>(fz_rt5))

(* Test: roundtrip nested *)
var rt6_b = $B.create()
val () = $B.bput(rt6_b, "{\"x\":[1,{\"y\":null}],\"z\":false}")
val @(rt6_a, _) = $B.to_arr(rt6_b)
val @(fz_rt6, bv_rt6) = $A.freeze<byte>(rt6_a)
val rt6_r = parse(bv_rt6, 0, 524288)
val () = (case+ rt6_r of
  | ~$R.ok(@(v, _)) => json_free(v)
  | ~$R.err(_) => ())
val () = $A.drop<byte>(fz_rt6, bv_rt6)
val () = $A.free<byte>($A.thaw<byte>(fz_rt6))

end
