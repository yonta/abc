structure Array31 : MINIMUM_ARRAY =
struct
  datatype 'a array =
           Raw of 'a Array.array
         | TwoLevel of 'a Array.array vector
  val maxLen = 0x7fffffff      (* 2^31 - 1 *)
  val arrayMaxLen = 0x1ffffff  (* SML# compiler limitation *)
  val maxVectorLen =           (* maxLen / maxVectorLen + 0 or 1 *)
      maxLen div arrayMaxLen + Int.min (1, maxLen mod arrayMaxLen)
  fun calcIndex index =
      let
        val vecIndex = index div arrayMaxLen
        val arrIndex = index mod arrayMaxLen
      in
        (vecIndex, arrIndex)
      end
  fun array (len, elem) =
      if len > maxLen then raise Size
      else if len <= arrayMaxLen then Raw (Array.array (len, elem))
      else
        let
          val (vi, ai) = calcIndex (len - 1)
          val vecNum = vi + 1
          val lastArrNum = ai + 1
          fun makeArray index =
              if index = vi then Array.array (lastArrNum, elem)
              else Array.array (arrayMaxLen, elem)
          val vec = Vector.tabulate (vecNum, makeArray)
        in
          TwoLevel vec
        end
  fun sub (Raw arr, index) = Array.sub (arr, index)
    | sub (TwoLevel arrvec, index) =
      if index + 1 > maxLen then raise Size
      else
        let
          val (vecIndex, arrIndex) = calcIndex index
          val arr = Vector.sub (arrvec, vecIndex)
        in
          Array.sub (arr, arrIndex)
        end
  fun update (Raw arr, index, elem) = Array.update (arr, index, elem)
    | update (TwoLevel arrvec, index, elem) =
      if index + 1 > maxLen then raise Size
      else
        let
          val (vecIndex, arrIndex) = calcIndex index
          val arr = Vector.sub (arrvec, vecIndex)
        in
          Array.update (arr, arrIndex, elem)
        end
end
