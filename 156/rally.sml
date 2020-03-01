fun ** reader input =
    case reader input of
        NONE => NONE
      | SOME (result, rest) =>
        case ** reader rest of
            NONE => SOME ([result], rest)
          | SOME (results, rest2) => SOME (result :: results, rest2)


fun sum l = foldl (op +) 0.0 l
fun calcPoint l = Real.fromInt (round (sum l / (Real.fromInt (length l))))
fun calcPower point x = Math.pow (x - point, 2.0)
fun calcMinPower l =
    let
      val point = calcPoint l
      val powers = map (calcPower point) l
    in
      sum powers
    end

fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val intReader = Int.scan StringCvt.DEC getc
      val (n, instream) = valOf (intReader instream)
                          handle Option.Option =>
                                 raise Fail "bug: invalid input N"
      val parse = ** intReader
      val (inputs, instream) = valOf (parse instream)
                               handle Option.Option => raise Fail "bug: invalid input Xs"
      val digit = calcMinPower (map Real.fromInt inputs)
      val () = TextIO.setInstream (io, instream)
    in
      print (Real.fmt (StringCvt.FIX (SOME 0)) digit)
    end
val () = main ()
