#include "share/atspre_staload.hats"
#use array as A
#use builder as B
#use json as J
#use result as R

(* parse_text on whole texts: every escape, surrogate pairs and lone
   surrogates (U+FFFD), UTF-8, control bytes, every number form JSON's
   grammar allows and the ones it does not, the structure (commas,
   colons, trailing data), and each parse_error with its offset.
   Then serialize: escaping, \ufffd for bytes that are not UTF-8, null
   for a lexeme that is not a number, and round trips.
   One line per check; exits 1 on any failure. *)

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

macdef NO_INT = ~999999

(* Whether a[0, k) is e[0, k) *)
fun same {la,le:agz}{c:pos}{k:nat | k <= c; k <= $B.BUILDER_CAP}{i:nat | i <= k} .<k - i>.
  (a: !$A.arr(byte, la, c), e: !$A.arr(byte, le, $B.BUILDER_CAP), i: int i, k: int k): bool =
  if i >= k then true
  else if byte2int0($A.get<byte>(a, i)) = byte2int0($A.get<byte>(e, i)) then same(a, e, i + 1, k)
  else false

(* Whether a[0, k) holds exactly the en bytes of e *)
fn bytes_are {la,le:agz}{c:pos}{k:nat | k <= c}{en:nat | en <= $B.BUILDER_CAP}
  (a: !$A.arr(byte, la, c), k: int k, e: !$A.arr(byte, le, $B.BUILDER_CAP), en: int en): bool =
  if k = en then same(a, e, 0, k) else false

(* What v is: 0 null, 1 true, 2 false, 3 number, 4 string, 5 array,
   6 object; the number's int (NO_INT when it has none) or the
   string's length; and whether the number's lexeme or the string's
   bytes are exactly e[0, en) *)
fn kind_of {sz:nat}{le:agz}{en:nat | en <= $B.BUILDER_CAP}
  (v: !$J.json(sz), e: !$A.arr(byte, le, $B.BUILDER_CAP), en: int en): @(int, int, bool) =
  case+ v of
  | $J.json_null() => @(0, 0, true)
  | $J.json_bool(t) => @((if t then 1 else 2), 0, true)
  | $J.json_num(lx, len, $R.some(i)) => @(3, i, bytes_are(lx, len, e, en))
  | $J.json_num(lx, len, $R.none()) => @(3, NO_INT, bytes_are(lx, len, e, en))
  | $J.json_str(s, len) => @(4, len, bytes_are(s, len, e, en))
  | $J.json_arr(_) => @(5, 0, true)
  | $J.json_obj(_) => @(6, 0, true)

(* What the text in b parses to, as kind_of says against ex, or the
   error's code and offset *)
fn outcome {n:pos | n <= $B.BUILDER_CAP}{m:nat | m <= $B.BUILDER_CAP}
  (b: $B.builder(n), ex: $B.builder(m)): @(int, int, bool) = let
  val @(arr, n) = $B.to_arr(b)
  val @(ea, en) = $B.to_arr(ex)
  val @(f, bv) = $A.freeze<byte>(arr)
  val @(text, rest) = $A.borrow_split<byte>(f, bv, n)
  val r = (case+ $J.parse_text(text, n) of
    | ~$R.ok(v) => let
        val r = kind_of(v, ea, en)
        val () = $J.json_free(v)
        val () = $A.free<byte>(ea)
      in r end
    | ~$R.err(e) => let
        val @(c, p) = err_code(e)
        val () = $A.free<byte>(ea)
      in @(c, p, true) end): @(int, int, bool)
  val () = $A.drop<byte>(f, $A.borrow_join<byte>(f, text, rest))
  val () = $A.free<byte>($A.thaw<byte>(f))
in r end

fn report (name: string, ok: bool): bool = let
  val () = (if ok then println! ("ok   ", name) else println! ("FAIL ", name))
in ok end

fn lit {sn:pos | sn <= 256} (s: string sn): $B.builder(sn) = let
  val b = $B.create()
  val () = $B.bput(b, s)
in b end

fn lit0 {sn:nat | sn <= 256} (s: string sn): $B.builder(sn) = let
  val b = $B.create()
  val () = $B.bput(b, s)
in b end

(* s parses to a value of kind code, with x as outcome gives it *)
fn is {sn:pos | sn <= 256} (s: string sn, code: int, x: int): bool = let
  val @(c, v, _) = outcome(lit(s), lit0(""))
in c = code && v = x end

(* s parses to a string of exactly the bytes of e *)
fn str_is {sn:pos | sn <= 256}{en:nat | en <= 256} (s: string sn, e: string en): bool = let
  val eb = lit0(e)
  val en = $B.length(eb)
  val @(c, v, same) = outcome(lit(s), eb)
in c = 4 && v = en && same end

(* s parses to a number whose lexeme is s itself, with int i *)
fn num_is {sn:pos | sn <= 256} (s: string sn, i: int): bool = let
  val @(c, v, same) = outcome(lit(s), lit0(s))
in c = 3 && v = i && same end

(* pre, the byte x, post: kind code with x as outcome gives it *)
fn with_byte {an:nat | an <= 64}{bn:nat | bn <= 64}{x:nat | x < 256}
  (pre: string an, x: int x, post: string bn, code: int, want: int): bool = let
  val b = $B.create()
  val () = $B.bput(b, pre)
  val () = $B.put_byte(b, x)
  val () = $B.bput(b, post)
  val @(c, v, _) = outcome(b, lit0(""))
in c = code && v = want end

(* Whether cs is one chunk holding exactly e[0, en) *)
fn one_chunk_is {k:nat}{le:agz}{en:nat | en <= $B.BUILDER_CAP}
  (cs: !$B.rope_list(k), e: !$A.arr(byte, le, $B.BUILDER_CAP), en: int en): bool =
  case+ cs of
  | $B.rope_cons(a, an, $B.rope_nil()) => bytes_are(a, an, e, en)
  | _ => false

(* v serializes to exactly s, and that parses back *)
fn ser_is {sz:nat | sz <= 4096}{sn:pos | sn <= 256} (v: $J.json(sz), s: string sn): bool = let
  val o = $B.create()
  val () = $J.serialize(v, o)
  val r = $B.rope_create()
  val () = $J.serialize_rope(v, r)
  val () = $J.json_free(v)
  val () = $B.rope_append(r, o)
  (* The rope now holds the rope's text, then the builder's: the same
     text twice *)
  val e = $B.create()
  val () = $B.bput(e, s)
  val () = $B.bput(e, s)
  val @(ea, en) = $B.to_arr(e)
  val cs = $B.rope_chunks(r)
  val ok = one_chunk_is(cs, ea, en)
  val () = $B.rope_list_free(cs)
  val () = $A.free<byte>(ea)
  val @(c, _, _) = outcome(lit(s), lit0(""))
in ok && c >= 0 end

(* A string value holding the k bytes x0 x1 x2 (k <= 3) *)
fn str_of {k:int | 1 <= k; k <= 3}{a,b,c:nat | a < 256; b < 256; c < 256}
  (k: int k, x0: int a, x1: int b, x2: int c): [sz:nat | sz <= 20] $J.json(sz) = let
  val arr = $A.alloc<byte>(3)
  val () = $A.write_byte(arr, 0, x0)
  val () = $A.write_byte(arr, 1, x1)
  val () = $A.write_byte(arr, 2, x2)
in $J.json_str(arr, k) end

(* A string value holding the four bytes x0 x1 x2 x3 *)
fn str4_of {a,b,c,d:nat | a < 256; b < 256; c < 256; d < 256}
  (x0: int a, x1: int b, x2: int c, x3: int d): [sz:nat | sz <= 26] $J.json(sz) = let
  val arr = $A.alloc<byte>(4)
  val () = $A.write_byte(arr, 0, x0)
  val () = $A.write_byte(arr, 1, x1)
  val () = $A.write_byte(arr, 2, x2)
  val () = $A.write_byte(arr, 3, x3)
in $J.json_str(arr, 4) end

(* parse at offset 2 of s: whether it gives an array ending at e *)
fn parse_at {sn:pos | sn <= 256} (s: string sn, e: int): bool = let
  val b = $B.create()
  val () = $B.bput(b, s)
  val @(arr, n) = $B.to_arr(b)
  val @(f, bv) = $A.freeze<byte>(arr)
  val ok = (case+ $J.parse(bv, 2, 524288) of
    | ~$R.ok(@(v, ep)) => let
        val k = (case+ v of $J.json_arr(_) => true | _ => false): bool
        val () = $J.json_free(v)
      in k && ep = e end
    | ~$R.err(x) => let val _ = $J.parse_error_pos(x) in false end): bool
  val () = $A.drop<byte>(f, bv)
  val () = $A.free<byte>($A.thaw<byte>(f))
in ok end

(* A number value with the lexeme of s and no int *)
fn num_of {sn:pos | sn <= 16} (s: string sn): [sz:nat | sz <= 20] $J.json(sz) = let
  val b = $B.create()
  val () = $B.bput(b, s)
  val @(src, n) = $B.to_arr(b)
  val arr = $A.alloc<byte>(n)
  fun copy {la,lb:agz}{i:nat | i <= sn} .<sn - i>.
    (src: !$A.arr(byte, la, $B.BUILDER_CAP), dst: !$A.arr(byte, lb, sn), i: int i, n: int sn): void =
    if i >= n then ()
    else let val () = $A.set<byte>(dst, i, $A.get<byte>(src, i)) in copy(src, dst, i + 1, n) end
  val () = copy(src, arr, 0, n)
  val () = $A.free<byte>(src)
in $J.json_num(arr, n, $R.none()) end

implement main0 () = let
  (* --- escapes ------------------------------------------------- *)
  val e1 = report("escape \\\"", str_is("\"\\\"\"", "\""))
  val e2 = report("escape \\\\", str_is("\"\\\\\"", "\\"))
  val e3 = report("escape \\/", str_is("\"\\/\"", "/"))
  val e4 = report("escape \\b", str_is("\"\\b\"", "\b"))
  val e5 = report("escape \\f", str_is("\"\\f\"", "\f"))
  val e6 = report("escape \\n", str_is("\"\\n\"", "\n"))
  val e7 = report("escape \\r", str_is("\"\\r\"", "\r"))
  val e8 = report("escape \\t", str_is("\"\\t\"", "\t"))
  val e9 = report("all escapes in a row", str_is("\"a\\\"\\\\\\/\\b\\f\\n\\r\\tz\"", "a\"\\/\b\f\n\r\tz"))
  val e10 = report("\\u0041 is A", str_is("\"\\u0041\"", "A"))
  val e11 = report("\\u0000 is a NUL byte", let
      val @(c, v, _) = outcome(lit("\"\\u0000\""), lit0(""))
    in c = 4 && v = 1 end)
  val e12 = report("\\u00e9 is two bytes", str_is("\"\\u00e9\"", "\303\251"))
  val e13 = report("\\u00E9 upper-case hex", str_is("\"\\u00E9\"", "\303\251"))
  val e14 = report("\\u20ac is three bytes", str_is("\"\\u20ac\"", "\342\202\254"))
  val e15 = report("\\u007f and \\u0080 and \\u07ff and \\u0800 and \\uffff",
    str_is("\"\\u007f\\u0080\\u07ff\\u0800\\uffff\"", "\177\302\200\337\277\340\240\200\357\277\277"))
  val e16 = report("surrogate pair \\ud83d\\ude00 is U+1F600", str_is("\"\\ud83d\\ude00\"", "\360\237\230\200"))
  val e17 = report("surrogate pair \\udbff\\udfff is U+10FFFF", str_is("\"\\udbff\\udfff\"", "\364\217\277\277"))
  val e18 = report("surrogate pair \\ud800\\udc00 is U+10000", str_is("\"\\ud800\\udc00\"", "\360\220\200\200"))
  (* Lone surrogates: each is U+FFFD (EF BF BD) *)
  val l1 = report("lone high at the end", str_is("\"a\\ud800\"", "a\357\277\275"))
  val l2 = report("lone low", str_is("\"\\udc00b\"", "\357\277\275b"))
  val l3 = report("high then a letter", str_is("\"\\ud83dx\"", "\357\277\275x"))
  val l4 = report("high then a non-low escape", str_is("\"\\ud83d\\u0041\"", "\357\277\275A"))
  val l5 = report("two highs then a low", str_is("\"\\ud83d\\ud83d\\ude00\"", "\357\277\275\360\237\230\200"))
  val l6 = report("low then high", str_is("\"\\ude00\\ud83d\"", "\357\277\275\357\277\275"))
  val l7 = report("high then \\n", str_is("\"\\ud83d\\n\"", "\357\277\275\n"))
  (* --- raw bytes ---------------------------------------------- *)
  val u1 = report("raw UTF-8 two, three and four bytes", str_is("\"\303\251\342\202\254\360\237\230\200\"", "\303\251\342\202\254\360\237\230\200"))
  val u2 = report("raw DEL", str_is("\"\177\"", "\177"))
  val u3 = report("raw U+2028", str_is("\"\342\200\250\"", "\342\200\250"))
  val u4 = report("lone continuation byte", with_byte("\"a", 128, "\"", ~9, 2))
  val u5 = report("overlong C0 80", is("\"\300\200\"", ~9, 1))
  val u6 = report("overlong E0 80 80", is("\"\340\200\200\"", ~9, 1))
  val u7 = report("encoded surrogate ED A0 80", is("\"\355\240\200\"", ~9, 1))
  val u8 = report("past U+10FFFF: F4 90 80 80", is("\"\364\220\200\200\"", ~9, 1))
  val u9 = report("F5", is("\"\365\200\200\200\"", ~9, 1))
  val u10 = report("FF", with_byte("\"", 255, "\"", ~9, 1))
  val u11 = report("truncated E2 82", is("\"\342\202\"", ~9, 1))
  val u12 = report("truncated at the end of input", is("\"\342\202", ~9, 1))
  val u13 = report("overlong F0 80 80 80", is("\"\360\200\200\200\"", ~9, 1))
  val c1 = report("raw NUL rejected", with_byte("\"a", 0, "\"", ~6, 2))
  val c2 = report("raw 0x1f rejected", with_byte("\"", 31, "\"", ~6, 1))
  val c3 = report("raw newline rejected", with_byte("\"ab", 10, "\"", ~6, 3))
  val c4 = report("raw tab rejected", with_byte("\"", 9, "\"", ~6, 1))
  val c5 = report("raw control in a key", with_byte("{\"", 1, "\":1}", ~6, 2))
  (* --- bad escapes -------------------------------------------- *)
  val b1 = report("\\x", is("\"a\\x\"", ~7, 2))
  val b2 = report("\\U", is("\"\\U0041\"", ~7, 1))
  val b3 = report("\\'", is("\"\\'\"", ~7, 1))
  val b4 = report("\\0", is("\"\\0\"", ~7, 1))
  val b5 = report("\\ then a space", is("\"\\ \"", ~7, 1))
  val b6 = report("\\u12G4", is("\"\\u12G4\"", ~8, 1))
  val b7 = report("\\u12 then the quote", is("\"\\u12\"", ~8, 1))
  val b8 = report("\\u at the end of input", is("\"\\u12", ~1, 5))
  val b9 = report("\\ at the end of input", is("\"\\", ~1, 2))
  val b10 = report("high then a bad escape", is("\"\\ud83d\\uZZZZ\"", ~8, 7))
  val b11 = report("high then a truncated escape", is("\"\\ud83d\\u12", ~1, 11))
  val b12 = report("bad escape in a key", is("{\"\\q\":1}", ~7, 2))
  val b13 = report("unterminated string", is("\"abc", ~1, 4))
  (* --- numbers ------------------------------------------------ *)
  val n1 = report("0", num_is("0", 0))
  val n2 = report("-0", num_is("-0", 0))
  val n3 = report("7", num_is("7", 7))
  val n4 = report("-42", num_is("-42", ~42))
  val n5 = report("int max", num_is("2147483647", 2147483647))
  val n6 = report("int min", num_is("-2147483648", ~2147483647 - 1))
  val n7 = report("past int max: no int", num_is("2147483648", NO_INT))
  val n8 = report("past int min: no int", num_is("-2147483649", NO_INT))
  val n9 = report("long integer", num_is("123456789012345678901234567890", NO_INT))
  val n10 = report("0.5", num_is("0.5", NO_INT))
  val n11 = report("-1.25", num_is("-1.25", NO_INT))
  val n12 = report("1e5", num_is("1e5", NO_INT))
  val n13 = report("1E5", num_is("1E5", NO_INT))
  val n14 = report("1e+5", num_is("1e+5", NO_INT))
  val n15 = report("1e-5", num_is("1e-5", NO_INT))
  val n16 = report("-0.0e-0", num_is("-0.0e-0", NO_INT))
  val n17 = report("1.5E+10", num_is("1.5E+10", NO_INT))
  val n18 = report("0e0", num_is("0e0", NO_INT))
  val n19 = report("long fraction", num_is("3.14159265358979323846264338327950288419716939937510", NO_INT))
  val n20 = report("1e400", num_is("1e400", NO_INT))
  val n21 = report("1.0 has no int", num_is("1.0", NO_INT))
  val n22 = report("number in an array", is("[ -1.5e3 , 2 ]", 5, 0))
  val m1 = report("01 leading zero", is("01", ~3, 0))
  val m2 = report("-01", is("-01", ~3, 0))
  val m3 = report("00", is("00", ~3, 0))
  val m4 = report("lone -", is("-", ~3, 0))
  val m5 = report("- then a space", is("- 1", ~3, 0))
  val m6 = report(".5", is(".5", ~2, 0))
  val m7 = report("1.", is("1.", ~3, 0))
  val m8 = report("1.e5", is("1.e5", ~3, 0))
  val m9 = report("+1", is("+1", ~2, 0))
  val m10 = report("1e", is("1e", ~3, 0))
  val m11 = report("1e+", is("1e+", ~3, 0))
  val m12 = report("NaN", is("NaN", ~2, 0))
  val m13 = report("Infinity", is("Infinity", ~2, 0))
  val m14 = report("-Infinity", is("-Infinity", ~3, 0))
  val m15 = report("0x10: trailing x", is("0x10", ~11, 1))
  val m16 = report("1.5.3", is("1.5.3", ~11, 3))
  val m17 = report("leading zero in an array", is("[01]", ~3, 1))
  val m18 = report("-.5", is("-.5", ~3, 0))
  val m19 = report("1e5.0", is("1e5.0", ~11, 3))
  (* --- structure ---------------------------------------------- *)
  val s1 = report("empty array", is("[]", 5, 0))
  val s2 = report("empty object", is("{}", 6, 0))
  val s3 = report("blanks around", is(" \t\r\n[ 1 , { \"a\" : [ ] } ] \n", 5, 0))
  val s4 = report("missing comma in array", is("[1 2]", ~2, 3))
  val s5 = report("trailing comma in array", is("[1,]", ~2, 3))
  val s6 = report("leading comma in array", is("[,1]", ~2, 1))
  val s7 = report("double comma", is("[1,,2]", ~2, 3))
  val s8 = report("missing comma in object", is("{\"a\":1 \"b\":2}", ~2, 7))
  val s9 = report("trailing comma in object", is("{\"a\":1,}", ~2, 7))
  val s10 = report("missing colon", is("{\"a\" 1}", ~2, 5))
  val s11 = report("key not a string", is("{a:1}", ~2, 1))
  val s12 = report("number key", is("{1:1}", ~2, 1))
  val s13 = report("unclosed array", is("[1", ~1, 2))
  val s14 = report("unclosed object", is("{\"a\":1", ~1, 6))
  val s15 = report("missing value", is("{\"a\":}", ~2, 5))
  val s16 = report("trailing data", is("1 2", ~11, 2))
  val s17 = report("two values", is("{}{}", ~11, 2))
  val s18 = report("tru", is("tru", ~1, 3))
  val s19 = report("trUe", is("trUe", ~2, 0))
  val s20 = report("nul", is("nul", ~1, 3))
  val s21 = report("falsy", is("falsy", ~2, 0))
  val s22 = report("empty input is only blanks", is(" ", ~1, 1))
  val s23 = report("mismatched brackets", is("[}", ~2, 1))
  val s24 = report("duplicate keys kept", is("{\"a\":1,\"a\":2}", 6, 0))
  val s25 = report("single quotes", is("'a'", ~2, 0))
  val s26 = report("comment", is("[1]//", ~11, 3))
  val s27 = report("false", is("false", 2, 0))
  val s28 = report("true", is("true", 1, 0))
  val s29 = report("null", is("null", 0, 0))
  val s30 = report("empty string", str_is("\"\"", ""))
  val s31 = report("empty key", is("{\"\":0}", 6, 0))
  val s32 = report("end after a key", is("{\"a\"", ~1, 4))
  val s33 = report("end after a comma in an object", is("{\"a\":1,", ~1, 7))
  val s34 = report("lone [", is("[", ~1, 1))
  val s35 = report("lone {", is("{", ~1, 1))
  val s36 = report("fals", is("fals", ~1, 4))
  val s37 = report("nulx", is("nulx", ~2, 0))
  val s38 = report("parse at an offset stops after the value", parse_at("xx[1] yy", 5))
  (* --- serialize ---------------------------------------------- *)
  val z1 = report("serialize escapes quote, backslash, newline", ser_is(str_of(3, 34, 92, 10), "\"\\\"\\\\\\n\""))
  val z2 = report("serialize \\b \\f \\r \\t", ser_is($J.json_arr($J.json_list_cons(str_of(2, 8, 12, 0),
    $J.json_list_cons(str_of(2, 13, 9, 0), $J.json_list_nil()))), "[\"\\b\\f\",\"\\r\\t\"]"))
  val z3 = report("serialize other controls as \\u00XX", ser_is(str_of(3, 0, 31, 27), "\"\\u0000\\u001f\\u001b\""))
  val z4 = report("serialize DEL and / raw", ser_is(str_of(2, 127, 47, 0), "\"\177/\""))
  val z5 = report("serialize UTF-8 raw", ser_is(str_of(2, 195, 169, 0), "\"\303\251\""))
  val z15 = report("serialize raw three-byte UTF-8", ser_is(str_of(3, 226, 130, 172), "\"\342\202\254\""))
  val z16 = report("serialize raw four-byte UTF-8", ser_is(str4_of(240, 159, 152, 128), "\"\360\237\230\200\""))
  val z6 = report("serialize a lone continuation byte as \\ufffd", ser_is(str_of(2, 97, 128, 0), "\"a\\ufffd\""))
  val z7 = report("serialize a truncated sequence as \\ufffd", ser_is(str_of(2, 226, 130, 0), "\"\\ufffd\\ufffd\""))
  val z8 = report("serialize an encoded surrogate as \\ufffd", ser_is(str_of(3, 237, 160, 128), "\"\\ufffd\\ufffd\\ufffd\""))
  val z9 = report("serialize a number lexeme", ser_is(num_of("-1.5e+10"), "-1.5e+10"))
  val z10 = report("serialize a bad lexeme as null", ser_is(num_of("01"), "null"))
  val z11 = report("serialize another bad lexeme as null", ser_is(num_of("1.e5"), "null"))
  val z12 = report("serialize ints", ser_is($J.json_arr($J.json_list_cons($J.json_num_of_int(0),
    $J.json_list_cons($J.json_num_of_int(~7), $J.json_list_cons($J.json_num_of_int(2147483647),
    $J.json_list_nil())))), "[0,-7,2147483647]"))
  val z13 = report("serialize an object", ser_is($J.json_obj($J.json_entries_cons(let
      val k = $A.alloc<byte>(1) val () = $A.write_byte(k, 0, 107) in k end, 1,
    $J.json_null(), $J.json_entries_nil())), "{\"k\":null}"))
  val z14 = report("serialize an empty string", let
      val a = $A.alloc<byte>(1)
    in ser_is($J.json_str(a, 0), "\"\"") end)
in
  if e1 && e2 && e3 && e4 && e5 && e6 && e7 && e8 && e9 && e10 && e11 && e12 && e13 && e14 && e15 &&
     e16 && e17 && e18 && l1 && l2 && l3 && l4 && l5 && l6 && l7 &&
     u1 && u2 && u3 && u4 && u5 && u6 && u7 && u8 && u9 && u10 && u11 && u12 && u13 &&
     c1 && c2 && c3 && c4 && c5 &&
     b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 &&
     n1 && n2 && n3 && n4 && n5 && n6 && n7 && n8 && n9 && n10 && n11 && n12 && n13 && n14 &&
     n15 && n16 && n17 && n18 && n19 && n20 && n21 && n22 &&
     m1 && m2 && m3 && m4 && m5 && m6 && m7 && m8 && m9 && m10 && m11 && m12 && m13 && m14 &&
     m15 && m16 && m17 && m18 && m19 &&
     s1 && s2 && s3 && s4 && s5 && s6 && s7 && s8 && s9 && s10 && s11 && s12 && s13 && s14 &&
     s15 && s16 && s17 && s18 && s19 && s20 && s21 && s22 && s23 && s24 && s25 && s26 && s27 &&
     s28 && s29 && s30 && s31 && s32 && s33 && s34 && s35 && s36 && s37 && s38 &&
     z1 && z2 && z3 && z4 && z5 && z6 && z7 && z8 && z9 && z10 && z11 && z12 && z13 && z14 && z15 && z16
  then println! ("grammar: all cases pass")
  else exit_void(1)
end
