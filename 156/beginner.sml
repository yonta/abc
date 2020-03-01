infix &&
fun op && (reader1, reader2) input =
    case reader1 input of
        NONE => NONE
      | SOME (result1, rest) =>
        (case reader2 rest of
             NONE => NONE
           | SOME (result2, rest) => SOME ((result1, result2), rest))

fun calcInsideRate n r =
    if n >= 10 then r
    else r + (100 * (10 - n))

fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val intReader = Int.scan StringCvt.DEC getc
      val parse = intReader && intReader
      val ((n, r), instream) = valOf (parse instream)
                   handle Option.Option => raise Fail "bug: invalid input"
      val rate = calcInsideRate n r
      val () = TextIO.setInstream (io, instream)
    in
      print (Int.toString rate)
    end
val () = main ()
