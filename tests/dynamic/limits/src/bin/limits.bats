#include "share/atspre_staload.hats"
#use array as A
#use builder as B
#use json as J
#use result as R

(* What the fuel-bounded parser got wrong or refused: an array of 1501
   elements (the old parser gave up after 1000), 300 levels of nesting,
   the int bounds (the old parser overflowed silently past them, which
   is undefined; a number past them now parses, with no int), and
   serialization of each kind of value, checked against its text and
   parsed back.
   One line per check; exits 1 on any failure. *)

(* The result code of parsing b[0, n): -1 error, else 5 array, 3 a
   number with an int, 4 a number without one; the int's value; and the
   end *)
fn parse_buf {n:pos | n <= $B.BUILDER_CAP}
  (b: $B.builder(n)): @(int, int, int) = let
  val @(arr, n) = $B.to_arr(b)
  val @(f, bv) = $A.freeze<byte>(arr)
  val r = (case+ $J.parse(bv, 0, 524288) of
    | ~$R.ok(@(v, ep)) => let
        val cx = (case+ v of
          | $J.json_arr(_) => @(5, 0)
          | $J.json_num(_, _, $R.some(i)) => @(3, i)
          | $J.json_num(_, _, $R.none()) => @(4, 0)
          | _ => @(9, 0)): @(int, int)
        val () = $J.json_free(v)
      in @(cx.0, cx.1, ep) end
    | ~$R.err(e) => let val _ = $J.parse_error_pos(e) in @(~1, 0, ~1) end): @(int, int, int)
  val () = $A.drop<byte>(f, bv)
  val () = $A.free<byte>($A.thaw<byte>(f))
in r end

fun commas {n,k:nat | n + 2 * k <= $B.BUILDER_CAP} .<k>.
  (b: !$B.builder(n) >> $B.builder(n + 2 * k), k: int k): void =
  if k = 0 then () else let
    val () = $B.bput(b, "0,")
  in commas(b, k - 1) end

fun opens {n,k:nat | n + k <= $B.BUILDER_CAP} .<k>.
  (b: !$B.builder(n) >> $B.builder(n + k), c: int, k: int k): void =
  if k = 0 then () else let
    val () = $B.put_byte(b, (if c = 91 then 91 else 93): [v:nat | v < 256] int v)
  in opens(b, c, k - 1) end

fn report (name: string, ok: bool): bool = let
  val () = (if ok then println! ("ok   ", name) else println! ("FAIL ", name))
in ok end

fn num {sn:pos | sn <= 64} (s: string sn): @(int, int, int) = let
  val b = $B.create()
  val () = $B.bput(b, s)
in parse_buf(b) end

(* Whether a[i, k) and b[i, k) hold the same bytes *)
fun same {la,lb:agz}{i:nat | i <= 524288} .<524288 - i>.
  (a: !$A.arr(byte, la, $B.BUILDER_CAP), b: !$A.arr(byte, lb, $B.BUILDER_CAP), i: int i, k: int): bool =
  if i >= k then true
  else if i >= 524288 then false
  else if byte2int0($A.get<byte>(a, i)) = byte2int0($A.get<byte>(b, i)) then same(a, b, i + 1, k)
  else false

(* oa[0, on) equals ea[0, en), and parses back to its end *)
fn out_is {la,lb:agz}{on:nat}
  (oa: $A.arr(byte, la, $B.BUILDER_CAP), on: int on,
   ea: $A.arr(byte, lb, $B.BUILDER_CAP), en: int): bool = let
  val eq = on = en && same(oa, ea, 0, on)
  val () = $A.free<byte>(ea)
  val @(f, bv) = $A.freeze<byte>(oa)
  val back = (case+ $J.parse(bv, 0, 524288) of
    | ~$R.ok(@(w, ep)) => let val () = $J.json_free(w) in ep = on end
    | ~$R.err(e) => let val _ = $J.parse_error_pos(e) in false end): bool
  val () = $A.drop<byte>(f, bv)
  val () = $A.free<byte>($A.thaw<byte>(f))
in eq && back end

(* v serializes to exactly s, and that output parses back to its end *)
fn ser_is {sz:nat | sz <= 524288}{sn:pos | sn <= 256}
  (v: $J.json(sz), s: string sn): bool = let
  val o = $B.create()
  val () = $J.serialize(v, o)
  val () = $J.json_free(v)
  val @(oa, on) = $B.to_arr(o)
  val e = $B.create()
  val () = $B.bput(e, s)
  val @(ea, en) = $B.to_arr(e)
in out_is(oa, on, ea, en) end

(* A 3-byte string buffer holding a, b, c *)
fn str3 {a,b,c:nat | a < 256; b < 256; c < 256}
  (a: int a, b: int b, c: int c): [l:agz] $A.arr(byte, l, 3) = let
  val arr = $A.alloc<byte>(3)
  val () = $A.write_byte(arr, 0, a)
  val () = $A.write_byte(arr, 1, b)
  val () = $A.write_byte(arr, 2, c)
in arr end

implement main0 () = let
  val b1 = $B.create()
  val () = $B.put_byte(b1, 91)
  val () = commas(b1, 1501)
  val () = $B.bput(b1, "0]")
  val @(c1, _, e1) = parse_buf(b1)
  val r1 = report("array_1502_elements", c1 = 5 && e1 = 3005)
  val b2 = $B.create()
  val () = opens(b2, 91, 300)
  val () = opens(b2, 93, 300)
  val @(c2, _, e2) = parse_buf(b2)
  val r2 = report("nesting_300", c2 = 5 && e2 = 600)
  val @(c3, v3, _) = num("2147483647")
  val r3 = report("int_max", c3 = 3 && v3 = 2147483647)
  val @(c4, v4, _) = num("-2147483648")
  val r4 = report("int_min", c4 = 3 && v4 = ~2147483647 - 1)
  val @(c5, _, _) = num("2147483648")
  val r5 = report("int_max_plus_one_has_no_int", c5 = 4)
  val @(c6, _, _) = num("-99999999999999999999")
  val r6 = report("long_negative_has_no_int", c6 = 4)
  val r7 = report("serialize_null", ser_is($J.json_null(), "null"))
  val r8 = report("serialize_bools", ser_is($J.json_arr($J.json_list_cons($J.json_bool(true),
    $J.json_list_cons($J.json_bool(false), $J.json_list_nil()))), "[true,false]"))
  val r9 = report("serialize_ints", ser_is($J.json_arr($J.json_list_cons($J.json_num_of_int(0),
    $J.json_list_cons($J.json_num_of_int(~7), $J.json_list_cons($J.json_num_of_int(42), $J.json_list_nil())))),
    "[0,-7,42]"))
  val r10 = report("serialize_string", ser_is($J.json_str(str3(97, 10, 98), 3), "\"a\\nb\""))
  val r11 = report("serialize_nested", ser_is($J.json_obj($J.json_entries_cons(str3(120, 0, 0), 1,
    $J.json_arr($J.json_list_cons($J.json_num_of_int(1), $J.json_list_cons(
      $J.json_obj($J.json_entries_cons(str3(121, 0, 0), 1, $J.json_null(), $J.json_entries_nil())),
      $J.json_list_nil()))),
    $J.json_entries_cons(str3(122, 0, 0), 1, $J.json_bool(false), $J.json_entries_nil()))),
    "{\"x\":[1,{\"y\":null}],\"z\":false}"))
in
  if r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11 then ()
  else exit_void(1)
end
