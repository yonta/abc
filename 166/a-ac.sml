fun main () =
    let
      val input = TextIO.inputLine TextIO.stdIn
      val input = valOf input
                  handle Option.Option => raise Fail "bug: invalid input"
      val output = if input = "ABC\n"
                   then "ARC"
                   else if input ="ARC\n"
                   then "ABC" else ""
    in
      print output
    end
val () = main ()
