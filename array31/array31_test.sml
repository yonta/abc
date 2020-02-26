structure Array31Test =
struct
  fun maxLen () =
      let
        val expected = IntInf.toInt ((IntInf.pow (2, 31)) - 1)
        val actual = Array31.maxLen
      in
        SMLUnit.Assert.assertEqualInt expected actual
      end
  fun array01 () =
      let
        val _ = Array31.array (10, 0)
      in
        SMLUnit.Assert.assertTrue true
      end
  fun array02 () =
      let
        val _ = Array31.array (Array31.maxLen, 1)
      in
        SMLUnit.Assert.assertTrue true
      end
  fun array03 () =
      let
        val _ = Array31.array (Array31.maxLen, 1.0)
      in
        SMLUnit.Assert.assertTrue true
      end
  fun array04 () =
      let
        val _ = Array31.array (Array31.maxLen, IntInf.fromInt 1)
      in
        SMLUnit.Assert.assertTrue true
      end
  fun array05 () =
      let
        val _ = Array31.array (Array31.maxLen, SOME 1)
      in
        SMLUnit.Assert.assertTrue true
      end
  fun array06 () =
      let
        val _ = Array31.array (Array31.maxLen, #"a")
      in
        SMLUnit.Assert.assertTrue true
      end
  fun array07 () =
      let
        val _ = Array31.array (Array31.maxLen, "abc")
      in
        SMLUnit.Assert.assertTrue true
      end
  fun array08 () =
      let
        val _ = Array31.array (Array31.maxLen, true)
      in
        SMLUnit.Assert.assertTrue true
      end
  fun array09 () =
      let
        val _ = Array31.array (Array31.maxLen, [1, 2, 3])
      in
        SMLUnit.Assert.assertTrue true
      end
  fun array10 () =
      let
        val _ = Array31.array (Array31.maxLen, Array.fromList [1, 2, 3])
      in
        SMLUnit.Assert.assertTrue true
      end
  fun equality01 () =
      let
        val expected = 0
        val arr = Array31.array (10, expected)
        val actual = Array31.sub (arr, 9)
      in
        SMLUnit.Assert.assertEqualInt expected actual
      end
  fun equality02 () =
      let
        val expected = 0
        val arr = Array31.array (10, 1)
        val () = Array31.update (arr, 5, expected)
        val actual = Array31.sub (arr, 5)
      in
        SMLUnit.Assert.assertEqualInt expected actual
      end

  val suite =
      SMLUnit.Test.labelTests
        [
          ("array01", array01),
          ("array02", array02),
          ("array03", array03),
          ("array04", array04),
          ("array05", array05),
          ("array06", array06),
          ("array07", array07),
          ("array08", array08),
          ("array09", array09),
          ("array10", array10),
          ("max length", maxLen),
          ("equality01", equality01),
          ("equality02", equality02)
        ]
end

val suite = Array31Test.suite
val () = SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suite
