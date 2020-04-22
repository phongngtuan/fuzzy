package sandbox

import cats.Monoid
import cats.implicits._

trait Econ[A] {
  def economics(a: A): Economics
}

object Econ {
  implicit val foEcon: Econ[Fo] = new Econ[Fo] {
    override def economics(a: Fo): Economics = a.economics
  }

  implicit val ccpEcon: Econ[Ccp] = new Econ[Ccp] {
    override def economics(a: Ccp): Economics = a.economics
  }

  def apply[A](implicit econ: Econ[A]): Econ[A] = econ

  implicit final def econSyntax[A: Econ](a: A): EconOps[A] = new EconOps[A](a)

  final class EconOps[A: Econ](a: A) {
    def economics: Economics = Econ[A].economics(a)
  }

}

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

  import Econ._

  type Fos = List[Fo]
  type Ccps = List[Ccp]

  def powerset[A](as: List[A]): Iterator[List[A]] = {
    Iterator.range(as.size, 0, -1).flatMap(k => as.combinations(k))
  }

  def findSplitFos(mergedExecution: Ccp, candidates: Fos): List[(Match, Fos)] = {
    val candidateCombinations = powerset(candidates)
    candidateCombinations
      .filter(combi => combi.map(_.economics).combineAll == mergedExecution.economics)
      .map(matchedCandidates => (Match(matchedCandidates, List(mergedExecution)), candidates.filterNot(c => matchedCandidates.map(_.id).toSet.contains(c.id))))
      .toList
  }

  def findSplitFosPrune(mergedExecution: Ccp, candidates: Fos): List[(Match, Fos)] = {
    val matchingResults = findSplitPrune(mergedExecution, candidates)
    matchingResults.map { ms =>
      val unused = candidates.filterNot(c => ms.split.map(_.id).toSet.contains(c.id))
      (Match(ms.split, List(ms.merge)), unused)
    }
  }

  def matchingOneCcpToManyFos(fos: List[Fo], ccps: List[Ccp]): List[(List[Match], List[Fo], List[Ccp])] = {
    // split FO: 1 CCP to many FO
    val zero: List[(List[Match], Fos, Ccps)] = List((Nil, fos, Nil))
    val matchingResult = ccps.foldLeft(zero) { (acc, ccp) =>
      acc.flatMap { case (matched, unmatchedFos, unmatchedCcps) =>
        // not using this ccp at all
        List((matched, unmatchedFos, ccp :: unmatchedCcps)) ++
          findSplitFos(ccp, unmatchedFos).map { case (ma, fos) => (ma :: matched, fos, unmatchedCcps) }
      }
    }
    matchingResult
  }

  def findSplitCcps(mergedExecution: Fo, candidates: Ccps): List[(Match, Ccps)] = {
    val candidateCombinations = powerset(candidates)
    candidateCombinations
      .filter(combi => combi.map(_.economics).combineAll == mergedExecution.economics)
      .map(matchedCandidates => (Match(List(mergedExecution), matchedCandidates), candidates.filterNot(c => matchedCandidates.map(_.id).toSet.contains(c.id))))
      .toList
  }

  def findSplitCcpsPrune(mergedExecution: Fo, candidates: Ccps): List[(Match, Ccps)] = {
    val matchingResults = findSplitPrune(mergedExecution, candidates)
    matchingResults.map { ms =>
      val unused = candidates.filterNot(c => ms.split.map(_.id).toSet.contains(c.id))
      (Match(List(ms.merge), ms.split), unused)
    }
  }

  case class MergeSplit[A, B](merge: A, split: List[B])

  def findSplitPrune[A: Econ, B: Econ](merge: A, split: List[B]): List[MergeSplit[A, B]] = {
    val target = Econ[A].economics(merge)
    val candidates = split.sortBy(Econ[B].economics(_).quantity)

    var ans = List.empty[MergeSplit[A, B]]

    def loop(acc: List[B], economics: Economics, candidates: List[B], unused: List[B]): Unit = {
      if (economics == target) {
        ans = MergeSplit(merge, acc) :: ans
      } else if (economics.quantity > target.quantity || economics.amount > target.amount) {
        ()
      } else {
        candidates match {
          case h :: t =>
            loop(h :: acc, economics |+| h.economics, t, unused)
            loop(acc, economics, t, h :: unused)
          case _ => ()
        }
      }
    }

    loop(Nil, Monoid[Economics].empty, candidates, Nil)
    ans
  }

  def matchingOneFoToManyCcps(fos: List[Fo], ccps: List[Ccp]): List[(List[Match], List[Fo], List[Ccp])] = {
    val zero: List[(List[Match], Fos, Ccps)] = List((Nil, Nil, ccps))
    val matchingResult = fos.foldLeft(zero) { (acc, fo) =>
      acc.flatMap { case (matched, unmatchedFos, unmatchedCcps) =>
        // not using this ccp at all
        List((matched, fo :: unmatchedFos, unmatchedCcps)) ++
          findSplitCcps(fo, unmatchedCcps).map { case (ma, ccps) => (ma :: matched, unmatchedFos, ccps) }
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