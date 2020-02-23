infix &&
fun op && (reader1, reader2) input =
    case reader1 input of
        NONE => NONE
      | SOME (result1, rest) =>
        (case reader2 rest of
             NONE => NONE
           | SOME (result2, rest) => SOME ((result1, result2), rest))

fun calcDigit n k =
    let
      val n = Real.fromInt n
      val k = Real.fromInt k
    in
      Real.floor (Math.log10 n / Math.log10 k) + 1
    end

fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val intReader = Int.scan StringCvt.DEC getc
      val parse = intReader && intReader
      val ((n, k), instream) =
          valOf (parse instream)
          handle Option.Option => raise Fail "bug: invalid input"
      val digit = calcDigit n k
      val () = TextIO.setInstream (io, instream)
    in
      print (Int.toString digit)
    end
val () = main ()
