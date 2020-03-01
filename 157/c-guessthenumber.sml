infix &&
fun op && (reader1, reader2) input =
    case reader1 input of
        NONE => NONE
      | SOME (result1, rest) =>
        (case reader2 rest of
             NONE => NONE
           | SOME (result2, rest) => SOME ((result1, result2), rest))

type constraint = (int * int option) list
exception Invalid
fun makeNConstraint n =
    (List.tabulate (n, fn i => (i + 1, NONE)) : constraint) (* 1-origin *)
fun updateC (index, num) (orig as (i, NONE)) =
    if i = index then (i, SOME num) else orig
  | updateC (index, num) (orig as (i, SOME n)) =
    if i = index andalso not (n = num) then raise Invalid else orig
fun updateConstraint constraint c = map (updateC c) constraint
fun setConstraint lineReader instream constraint =
    case lineReader instream of
        NONE => constraint
      | SOME (c as (index, num), instream) =>
        let val constraint = updateConstraint constraint c
        in  setConstraint lineReader instream constraint end
fun makeSmallestNumber nil = nil
  | makeSmallestNumber [(1, NONE)] = [0]
  | makeSmallestNumber [(1, SOME n)] = [n]
  | makeSmallestNumber ((1, NONE) :: cs) = 1 :: makeSmallestNumber cs
  | makeSmallestNumber ((1, SOME 0) :: _) = raise Invalid
  | makeSmallestNumber ((1, SOME n) :: cs) = n :: makeSmallestNumber cs
  | makeSmallestNumber ((index, NONE) :: cs) = 0 :: makeSmallestNumber cs
  | makeSmallestNumber ((index, SOME num) :: cs) = num :: makeSmallestNumber cs

fun main () =
    let
      val io = TextIO.stdIn
      val instream = TextIO.getInstream io
      val getc = TextIO.StreamIO.input1
      val intReader = Int.scan StringCvt.DEC getc
      val lineReader = intReader && intReader
      val ((n, m), instream) = valOf (lineReader instream)
                               handle Option.Option
                                      => raise Fail "bug: invalid input"
      val constraint = makeNConstraint n
      val constraint = setConstraint lineReader instream constraint
      val () = TextIO.setInstream (io, instream)
      val numbers = makeSmallestNumber constraint
    in
      app (print o Int.toString) numbers
    end
    handle Invalid => print "-1"
val () = main ()
