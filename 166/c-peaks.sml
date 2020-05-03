infix &&
fun op && (reader1, reader2) input =
    case reader1 input of
        NONE => NONE
      | SOME (result1, rest) =>
        (case reader2 rest of
             NONE => NONE
           | SOME (result2, rest) => SOME ((result1, result2), rest))
fun nth 0 reader input = SOME (nil, input)
  | nth n reader input =
    case reader input of
        NONE => NONE
      | SOME (result1, rest1) =>
        case nth (n - 1) reader rest1 of
            NONE => NONE
          | SOME (result2, rest2) => SOME (result1 :: result2, rest2)

fun addEdges arr nil = ()
  | addEdges arr ((node1, node2) :: rest) =
    let
      val (id1, id2) = (node1 - 1, node2 - 1) (* zero origin *)
      val () = Array.update (arr, id1, id2 :: Array.sub (arr, id1))
      val () = Array.update (arr, id2, id1 :: Array.sub (arr, id2))
    in
      addEdges arr rest
    end
fun isHighest id height heightsVec edgesArr =
    let
      val toIndexes = Array.sub (edgesArr, id)
    in
      List.all
        (fn toIndex => height > Vector.sub (heightsVec, toIndex))
        toIndexes
    end
fun countTrue vec = Vector.foldl (fn (true, n) => n + 1 | (false, n) => n) 0 vec

fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val intReader = Int.scan StringCvt.DEC getc
      val ((nodeNum, edgeNum), instream) =
          valOf ((intReader && intReader) instream)
          handle Option.Option => raise Fail "bug: invalid input N & M"
      val (heights, instream) =
          valOf (nth nodeNum intReader instream)
          handle Option.Option => raise Fail "bug: invalid input Hs"
      val heightsVec = Vector.fromList heights
      val (edges, instream) =
          valOf (nth edgeNum (intReader && intReader) instream)
          handle Option.Option => raise Fail "bug: invalid input A & B"
      val () = TextIO.setInstream (io, instream)
      val edgesArr = Array.tabulate (nodeNum, fn _ => nil)
      val () = addEdges edgesArr edges
      val highests =
          Vector.mapi
            (fn (id, height) => isHighest id height heightsVec edgesArr)
            heightsVec
      val answer = countTrue highests
    in
      print (Int.toString answer)
    end

val () = main ()
