fun listReader nil input = SOME (nil, input)
  | listReader (reader :: readers) input =
    case reader input of
        NONE => NONE
      | SOME (result, rest) =>
        case listReader readers rest of
            NONE => NONE
          | SOME (results, rest2) => SOME (result :: results, rest2)
fun ** reader input =
    case reader input of
        NONE => NONE
      | SOME (result, rest) =>
        case ** reader rest of
            NONE => SOME ([result], rest)
          | SOME (results, rest2) => SOME (result :: results, rest2)

type line = (int * bool) list
type bingo = line list
fun makeBingo nineXNine =
    map (fn line => map (fn num => (num, false)) line) nineXNine
fun getFirst l = hd l
fun getSecond (_ :: x :: l) = x
  | getSecond _ = raise List.Empty
fun getThird (_ :: _ :: x :: l) = x
  | getThird _ = raise List.Empty
fun id x = x
fun checkVertical getter (bingo : bingo) = List.all id (map (#2 o getter) bingo)
fun checkVerticals bingo = (checkVertical getFirst bingo) orelse
                           (checkVertical getSecond bingo) orelse
                           (checkVertical getThird bingo)
fun checkHorizon (line : line) = List.all id (map #2 line)
fun checkHorizons bingo = List.exists id (map checkHorizon bingo)
(* TODO: Refactor *)
fun checkDiagonals ([[x1, x2, x3], [x4, x5, x6], [x7, x8, x9]] : bingo) =
    (#2 x1 andalso #2 x5 andalso #2 x9) orelse
    (#2 x3 andalso #2 x5 andalso #2 x7)
  | checkDiagonals _ = raise Fail "bug: invalid input"
fun checkBingo bingo = checkVerticals bingo orelse
                       checkHorizons bingo orelse
                       checkDiagonals bingo
fun openLine num (line : line)=
    map (fn (n, b) => if num = n then (n, true) else (n, b)) line
fun openNumber bingo num = map (openLine num) bingo
fun openNumberLoop intReader instream bingo =
    case intReader instream of
        NONE => false
      | SOME (num, instream) =>
        let
          val bingo = openNumber bingo num
        in
          if checkBingo bingo then true
          else openNumberLoop intReader instream bingo
        end

fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val intReader = Int.scan StringCvt.DEC getc
      val lineReader = listReader [intReader, intReader, intReader]
      val bingoReader = listReader [lineReader, lineReader, lineReader]
      val (initNums, instream) = valOf (bingoReader instream)
                            handle Option.Option
                                   => raise Fail "bug: invalid input"
      val bingo = makeBingo initNums
      val result = openNumberLoop intReader instream bingo
      val () = TextIO.setInstream (io, instream)
    in
      if result then print "Yes" else print "No"
    end
val () = main ()
