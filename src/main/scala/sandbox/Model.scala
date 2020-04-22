package sandbox

import cats.Monoid
import cats.implicits._

case class Economics(quantity: BigDecimal, amount: BigDecimal)

object Economics {
  implicit val economicsMonoid: Monoid[Economics] = new Monoid[Economics] {
    override def empty: Economics = Economics(0, 0)

    override def combine(x: Economics, y: Economics): Economics = Economics(x.quantity + y.quantity, x.amount + y.amount)
  }
}

case class Fo(id: String, instrument: String, economics: Economics)

case class Ccp(id: String, instrument: String, economics: Economics)

case class Match(fos: List[Fo], ccps: List[Ccp])

object FuzzyMatch {
  type Fos = List[Fo]
  type Ccps = List[Ccp]

  def powerset[A](as: List[A]): Iterator[List[A]] = {
    Iterator.range(as.size, 0, -1).flatMap(k => as.combinations(k))
  }

  def findMatchingFos(mergedExecution: Ccp, candidates: Fos): List[(Match, Fos)] = {
    val candidateCombinations = powerset(candidates)
    candidateCombinations
      .filter(combi => combi.map(_.economics).combineAll == mergedExecution.economics)
      .map(matchedCandidates => (Match(matchedCandidates, List(mergedExecution)), candidates.filterNot(c => matchedCandidates.map(_.id).toSet.contains(c.id))))
      .toList
  }

  def matchingOneCcpToManyFos(fos: List[Fo], ccps: List[Ccp]): List[(List[Match], List[Fo], List[Ccp])] = {
    // split FO: 1 CCP to many FO
    val zero: List[(List[Match], Fos, Ccps)] = List((Nil, fos, Nil))
    val matchingResult = ccps.foldLeft(zero) { (acc, ccp) =>
      acc.flatMap { case (matched, unmatchedFos, unmatchedCcps) =>
        // not using this ccp at all
        List((matched, unmatchedFos, ccp :: unmatchedCcps)) ++
          findMatchingFos(ccp, unmatchedFos).map { case (ma, fos) => (ma :: matched, fos, unmatchedCcps) }
      }
    }
    matchingResult
  }

  def findMatchingCcps(mergedExecution: Fo, candidates: Ccps): List[(Match, Ccps)] = {
    val candidateCombinations = powerset(candidates)
    candidateCombinations
      .filter(combi => combi.map(_.economics).combineAll == mergedExecution.economics)
      .map(matchedCandidates => (Match(List(mergedExecution), matchedCandidates), candidates.filterNot(c => matchedCandidates.map(_.id).toSet.contains(c.id))))
      .toList
  }

  def matchingOneFoToManyCcps(fos: List[Fo], ccps: List[Ccp]): List[(List[Match], List[Fo], List[Ccp])] = {
    val zero: List[(List[Match], Fos, Ccps)] = List((Nil, Nil, ccps))
    val matchingResult = fos.foldLeft(zero) { (acc, fo) =>
      acc.flatMap { case (matched, unmatchedFos, unmatchedCcps) =>
        // not using this ccp at all
        List((matched, fo :: unmatchedFos, unmatchedCcps)) ++
          findMatchingCcps(fo, unmatchedCcps).map { case (ma, ccps) => (ma :: matched, unmatchedFos, ccps) }
      }
    }
    matchingResult
  }


  def findMatches(fos: List[Fo], ccps: List[Ccp]): List[(List[Match], List[Fo], List[Ccp])] = {
    println(s"fo: ${fos.size}, ccp: ${ccps.size}")
    val ans = matchingOneCcpToManyFos(fos, ccps).flatMap { case (matched1, fos1, ccps1) =>
      matchingOneFoToManyCcps(fos1, ccps1).map { case (matched2, fos2, ccps2) =>
        (matched2 ++ matched1, fos2, ccps2)
      }
    }
    println(ans)
    ans
  }
}