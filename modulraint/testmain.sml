val arrayLen = Array.maxLen div 8 (* OK size, equals 0x3ffffff *)
val bigArray = Array.array (arrayLen, SOME 0)
val n = Array.sub (bigArray, arrayLen - 1)
val () = print (Int.toString (valOf n))
val () = print "\n"

val arrayLen = Array.maxLen div 4 (* OK size *)
val bigArray = Array.array (arrayLen, 0)
val n = Array.sub (bigArray, arrayLen - 1)
val () = print (Int.toString n)
val () = print "\n"

               (* NG
val arrayLen = Array.maxLen div 8 + 1    (* NG size *)
val bigArray = Array.array (arrayLen, SOME 0)
val n = Array.sub (bigArray, arrayLen - 1)
val () = print (Int.toString (valOf n))
val () = print "\n"
                *)
val bigArray = Array31.array (Array31.maxLen, SOME 0)
(* val n = ModularInt.pow 2 10 *)
