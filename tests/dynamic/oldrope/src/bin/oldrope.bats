#include "share/atspre_staload.hats"
#use array as A
#use builder as B
#use json as J

(* serialize_rope, the deprecated form on builder's old rope (area:
   overlap; removal: json#53), still serializes: this test calls it, so
   an edit cannot drop it before that ticket. serialize_sized_rope is
   tested by caps and grammar. *)

fun same {la,le:agz}{c:pos}{k:nat | k <= c; k <= $B.BUILDER_CAP}{i:nat | i <= k} .<k - i>.
  (a: !$A.arr(byte, la, c), e: !$A.arr(byte, le, $B.BUILDER_CAP), i: int i, k: int k): bool =
  if i >= k then true
  else if byte2int0($A.get<byte>(a, i)) = byte2int0($A.get<byte>(e, i))
  then same(a, e, i + 1, k)
  else false

fn one_chunk_is {k:nat}{le:agz}{en:nat | en <= $B.BUILDER_CAP}
  (cs: !$B.rope_list(k), e: !$A.arr(byte, le, $B.BUILDER_CAP), en: int en): bool =
  case+ cs of
  | $B.rope_cons(a, an, $B.rope_nil()) =>
      (if an = en then same(a, e, 0, an) else false)
  | _ => false

implement main0 () = let
  val v = $J.json_arr($J.json_list_cons($J.json_num_of_int(0),
    $J.json_list_cons($J.json_num_of_int(~7), $J.json_list_nil())))
  val r = $B.rope_create()
  val () = $J.serialize_rope(v, r)
  val () = $J.json_free(v)
  val e = $B.create()
  val () = $B.bput(e, "[0,-7]")
  val @(ea, en) = $B.to_arr(e)
  val cs = $B.rope_chunks(r)
  val ok = one_chunk_is(cs, ea, en)
  val () = $B.rope_list_free(cs)
  val () = $A.free<byte>(ea)
in
  if ok then println! ("oldrope: serialize_rope still works")
  else exit_void(1)
end
