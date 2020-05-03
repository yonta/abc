infix &&
fun op && (reader1, reader2) input =
    case reader1 input of
        NONE => NONE
      | SOME (result1, rest) =>
        (case reader2 rest of
             NONE => NONE
           | SOME (result2, rest) => SOME ((result1, result2), rest))
fun nthImpl 0 reader input = SOME (nil, input)
  | nthImpl n reader input =
    case reader input of
        NONE => NONE
      | SOME (result1, rest1) =>
        case nthImpl (n - 1) reader rest1 of
            NONE => NONE
          | SOME (result2, rest2) => SOME (result1 :: result2, rest2)
fun nth n reader input =
    if n < 0 then raise Domain else nthImpl n reader input
fun readerWithHeader reader instream =
    case reader instream of
        NONE => NONE
      | SOME (n, instream) => nth n reader instream

fun leaveTrick _ nil = ()
  | leaveTrick tricks (owner :: owners) =
    let
      val index = owner - 1   (* zero origin *)
      val () = Array.update (tricks, index, false)
    in
      leaveTrick tricks owners
    end
fun countTrue arr = Array.foldl (fn (true, n) => n + 1 | (false, n) => n) 0 arr

fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val intReader = Int.scan StringCvt.DEC getc
      val ((peopleNum, treatKind), instream) =
          valOf ((intReader && intReader) instream)
          handle Option.Option => raise Fail "bug: invalid input N & K"
      val (owners, instream) =
          valOf (nth treatKind (readerWithHeader intReader) instream)
          handle Option.Option => raise Fail "bug: invalid input d & As"
      val () = TextIO.setInstream (io, instream)
      val tricks = Array.tabulate (peopleNum, fn _ => true)
      val () = app (leaveTrick tricks) owners
      val answer = countTrue tricks
    in
      print (Int.toString answer)
    end

val () = main ()
