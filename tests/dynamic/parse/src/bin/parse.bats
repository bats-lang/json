#include "share/atspre_staload.hats"
#use array as A
#use json as J
#use result as R
#use str as S

(* parse on whole buffers. Result code: -1 error, 0 null, 1 true,
   2 false, 3 int, 4 string, 5 array, 6 object; plus the int value or
   string length, and the end position. Truncated inputs must be errors
   (the previous parser read past the end of the buffer on them).
   Exits 1 on any mismatch. *)
fn run {k:pos | k <= 1048576}
  (name: string, src: &(@[char][k]), k: int k, code: int, extra: int, endp: int): bool = let
  val @(f, b) = $A.freeze<byte>($S.from_char_array(src, k))
  val @(c, x, e) = (case+ $J.parse(b, 0, k) of
    | ~$R.ok(@(v, ep)) => let
        val @(c, x) = (case+ v of
          | $J.json_null() => @(0, 0)
          | $J.json_bool(t) => @((if t then 1 else 2), 0)
          | $J.json_int(i) => @(3, i)
          | $J.json_str(_, n) => @(4, n)
          | $J.json_arr(_) => @(5, 0)
          | $J.json_obj(_) => @(6, 0)): @(int, int)
        val () = $J.json_free(v)
      in @(c, x, ep) end
    | ~$R.err(_) => @(~1, 0, ~1)): @(int, int, int)
  val () = $A.drop<byte>(f, b)
  val () = $A.free<byte>($A.thaw<byte>(f))
  val ok = c = code && x = extra && (code < 0 || e = endp)
  val () = (if ok then () else println! ("FAIL ", name, ": code ", c, " extra ", x, " end ", e))
in ok end

implement main0 () = let
  var a = @[char][4]('n', 'u', 'l', 'l')
  var b = @[char][6](' ', ' ', 't', 'r', 'u', 'e')
  var c = @[char][3]('-', '4', '2')
  var d = @[char][6]('\042', 'a', '\134', 'n', 'b', '\042')
  var e = @[char][6]('\133', '1', ',', ' ', '2', '\135')
  var f = @[char][8]('\173', '\042', 'k', '\042', ':', ' ', '3', '\175')
  var t1 = @[char][3]('\133', '1', ',')
  var t2 = @[char][5]('\173', '\042', 'a', '\042', ':')
  var t3 = @[char][4]('\042', 'a', 'b', 'c')
  var t4 = @[char][3]('t', 'r', 'u')
  var t5 = @[char][2]('\042', '\134')
  var t6 = @[char][3]('\173', '\042', 'k')
  val r1 = run("null", a, 4, 0, 0, 4)
  val r2 = run("  true", b, 6, 1, 0, 6)
  val r3 = run("-42", c, 3, 3, ~42, 3)
  val r4 = run("\"a\\nb\"", d, 6, 4, 3, 6)
  val r5 = run("[1, 2]", e, 6, 5, 0, 6)
  val r6 = run("{\"k\": 3}", f, 8, 6, 0, 8)
  val r7 = run("truncated [1,", t1, 3, ~1, 0, 0)
  val r8 = run("truncated {\"a\":", t2, 5, ~1, 0, 0)
  val r9 = run("unterminated \"abc", t3, 4, ~1, 0, 0)
  val r10 = run("truncated tru", t4, 3, ~1, 0, 0)
  val r11 = run("backslash at end", t5, 2, ~1, 0, 0)
  val r12 = run("unterminated key", t6, 3, ~1, 0, 0)
in
  if r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11 && r12
  then println! ("parse: all cases pass")
  else exit_void(1)
end
