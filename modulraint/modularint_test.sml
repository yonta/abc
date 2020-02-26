structure ModularIntTest =
struct
  val modulo = IntInf.fromInt ModularInt.modulo

  fun pow01 () =
      let
        val expected = IntInf.toInt (IntInf.pow (2, 32) mod modulo)
        val actual = ModularInt.pow 2 32
      in
        SMLUnit.Assert.assertEqualInt expected actual
      end

  fun nCk01 () =
      let
        val expected = 20
        val actual = ModularInt.nCk 6 3
      in
        SMLUnit.Assert.assertEqualInt expected actual
      end
  fun nCk02 () =
      let
        val expected = 314928502
        val actual = ModularInt.nCk 1024 256
      in
        SMLUnit.Assert.assertEqualInt expected actual
      end

  fun inverse01 () =
      let
        val expected = ModularInt.pow 1000 (IntInf.toInt modulo - 2)
        val actual = ModularInt.inverse 1000
      in
        SMLUnit.Assert.assertEqualInt expected actual
      end

  fun factorial n =
      let
        val l = List.tabulate (n, fn i => IntInf.fromInt i + 1)
        val inf = foldl IntInf.* 1 l
        val inf = inf mod modulo
      in
        IntInf.toInt inf
      end
  fun factorial01 () =
      let
        val expected = factorial 10
        val actual = ModularInt.factorial 10
      in
        SMLUnit.Assert.assertEqualInt expected actual
      end
  fun factorial02 () =
      let
        val expected = factorial 1000
        val actual = ModularInt.factorial 1000
      in
        SMLUnit.Assert.assertEqualInt expected actual
      end

  val suite =
      SMLUnit.Test.labelTests
        [
          ("pow01", pow01),
          ("nCk01", nCk01),
          ("nCk02", nCk02),
          ("inverse01", inverse01),
          ("factorial01", factorial01),
          ("factorial02", factorial02)
        ]
end

val suite = ModularIntTest.suite
val () = SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suite
