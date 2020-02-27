val suite =
    SMLUnit.Test.TestList
      [
        SMLUnit.Test.TestLabel ("ModularInt", ModularIntTest.suite),
        SMLUnit.Test.TestLabel
          ("MemoizedModularInt", MemoizedModularIntTest.suite)
      ]
val () = SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suite
