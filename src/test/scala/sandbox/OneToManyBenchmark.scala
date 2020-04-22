package sandbox

import org.scalameter.api._

object RangeBenchmark
  extends Bench.LocalTime {
  val sizes = Gen.range("size")(5, 20, 1)
  val ranges = for {
    size <- sizes
  } yield TestDataGen.testData(5, size)

  performance of "Range" in {
    measure method "findMatchingCcps" in {
      using(ranges) in { case (fo, ccps) =>
        FuzzyMatch.findSplitCcps(fo, ccps)
      }
    }
    measure method "findMatchingCcpsParallel" in {
      using(ranges) in { case (fo, ccps) =>
        FuzzyMatch.findSplitCcpsPrune(fo, ccps)
      }
    }
  }
}
