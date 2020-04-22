package sandbox

;

import java.util.UUID

import cats.implicits._
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Gen, Properties}
import org.scalacheck.ScalacheckShapeless._

import scala.util.Random

object FuzzyMatchTest extends Properties("Matcher") {
  def randomId: String = UUID.randomUUID().toString
  val mergeSize = 5

  val economicsGen: Gen[Economics] = for {
    amount <- Gen.posNum[Int]
    quantity <- Gen.posNum[Int]
  } yield Economics(amount, quantity)

  val mergedCcp: Gen[(List[Fo], List[Ccp])] = for {
    size <- Gen.chooseNum(1, mergeSize)
    economics <- Gen.listOfN(size, economicsGen)
    instrument <- Gen.alphaNumStr
    fos = economics.mapWithIndex((economics, id) => Fo(id.toString, instrument, economics))
    ccps = List(Ccp("merged", instrument, economics.combineAll))
  } yield (fos, ccps)

  val mergedFo: Gen[(List[Fo], List[Ccp])] = for {
    size <- Gen.chooseNum(1, mergeSize)
    economics <- Gen.listOfN(size, economicsGen)
    instrument <- Gen.alphaNumStr
    ccps = economics.mapWithIndex((economics, id) => Ccp(id.toString, instrument, economics))
    fos = List(Fo("merged", instrument, economics.combineAll))
  } yield (fos, ccps)

  val singleMerge: Gen[(List[Fo], List[Ccp])] = Gen.oneOf(mergedCcp, mergedFo)
  val multipleMerge: Gen[List[(List[Fo], List[Ccp])]] = for {
    ccpMergeCount <- Gen.chooseNum(1, 5)
    ccpMerges <- Gen.listOfN(ccpMergeCount, mergedCcp)
    foMergeCount <- Gen.chooseNum(1, 5)
    foMerges <- Gen.listOfN(foMergeCount, mergedFo)
  } yield ccpMerges ++ foMerges
//
//  property("detect merged FO") = forAll(mergedFo) { case (fos, ccps) =>
//    val matchingResults = FuzzyMatch.matchingOneFoToManyCcps(fos, ccps)
//    matchingResults.exists { case (matched, remainingFo, remainingCcp) =>
//      matched.nonEmpty && remainingFo.isEmpty && remainingCcp.isEmpty
//    }
//  }
//
//  property("detect merged CCP") = forAll(mergedCcp) { case (fos, ccps) =>
//    val matchingResults = FuzzyMatch.matchingOneCcpToManyFos(fos, ccps)
//    matchingResults.exists { case (matched, remainingFo, remainingCcp) =>
//      matched.nonEmpty && remainingFo.isEmpty && remainingCcp.isEmpty
//    }
//  }
//
//  property("detect a split scenario") = forAll(singleMerge) { case (fos, ccps) =>
//    FuzzyMatch.findMatches(fos, ccps).size == Math.min(fos.length, ccps.length)
//  }

    property("multiple merges") = forAll(multipleMerge) { merges =>
      val (fos, ccps) = merges.mapWithIndex { (merged, id) =>
        val (fos, ccps) = merged
        (fos.map(e => e.copy(id = s"${id}-${e.id}")), ccps.map(e => e.copy(s"${id}-${e.id}")))
      }.unzip
      val flattenFos = Random.shuffle(fos.flatten)
      val flattenCcps = Random.shuffle(ccps.flatten)
      val matchingResults = FuzzyMatch.findMatches(flattenFos, flattenCcps)
      matchingResults.exists { case (matched, remainingFo, remainingCcp) =>
        matched.nonEmpty && remainingFo.isEmpty && remainingCcp.isEmpty
      }
    }

//  property("merged after a split must have save economics") = forAll(singleMerge) { case (fos, ccps) =>
//    val merged = FuzzyMatch.findMatches(fos, ccps)
//    merged.forall(m => m.fos.map(_.economics).combineAll == m.ccps.map(_.economics).combineAll)
//  }

  //
//  property("detect a split scenario with extra ccp") = forAll(singleMerge) { case (fos, ccps) =>
//    val duplicateCcps = ccps.zipWithIndex.map {
//      case (ccp, index) => ccp.copy(id = s"duplicate$index")
//    }
//    val (matched, _, remainingCcps) = FuzzyMatch.matchingOneCcpToManyFo(fos, ccps ++ duplicateCcps)
//    println(s"matched: ${matched.map(_.ccps)}, remainingCcp: $remainingCcps")
//    remainingCcps.size == ccps.size
//  }
}