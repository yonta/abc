fun calcPaper n = n div 2 + n mod 2
fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val intReader = Int.scan StringCvt.DEC getc
      val (page, instream) = valOf (intReader instream)
                            handle Option.Option
                                   => raise Fail "bug: invalid input"
      val () = TextIO.setInstream (io, instream)
      val paper = calcPaper page
    in
      print (Int.toString paper)
    end
val () = main ()
