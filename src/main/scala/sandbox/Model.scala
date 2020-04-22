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

  def findMatchingFos(ccp: Ccp, fos: Fos): Option[(Match, Fos)] = {
    @scala.annotation.tailrec
    def loop(ccp: Ccp, combinations: Iterator[Fos]): Option[Fos] = {
      combinations.nextOption() match {
        case Some(fos) =>
          if (ccp.economics == fos.map(_.economics).combineAll)
            Option(fos)
          else
            loop(ccp, combinations)
        case _ => None
      }
    }
    val foCombinations = powerset(fos)
    loop(ccp, foCombinations).map { matchedFos =>
        val matchedFoIds = matchedFos.map(_.id)
        val remainingFos = fos.filterNot(fo => matchedFoIds.contains(fo.id))
        (Match(matchedFos, List(ccp)), remainingFos)
    }
  }

  def findMatchingCcps(fo: Fo, ccps: Ccps): Option[(Match, Ccps)] = {
    @scala.annotation.tailrec
    def loop(fo: Fo, combinations: Iterator[Ccps]): Option[Ccps] = {
      combinations.nextOption() match {
        case Some(candidate) =>
          if (fo.economics == candidate.map(_.economics).combineAll)
            Option(candidate)
          else
            loop(fo, combinations)
        case _ => None
      }
    }
    loop(fo, powerset(ccps)).map { matches =>
      val matchedIds = matches.map(_.id)
      val remainingFos = ccps.filterNot(exec => matchedIds.contains(exec.id))
      (Match(List(fo), matches), remainingFos)
    }
  }

  def matchingOneCcpToManyFo(fos: List[Fo], ccps: List[Ccp]): (List[Match], List[Fo], List[Ccp]) = {
    // split FO: 1 CCP to many FO
    val acc: (List[Match], Fos, Ccps) = (Nil, fos, Nil)
    val matchingResult = ccps.foldLeft(acc) {
      case ((matched, foCandidates, unmatchedCcps), ccp) =>
        findMatchingFos(ccp, foCandidates) match {
          case Some((matching, remainingFos)) => (matching :: matched, remainingFos, unmatchedCcps)
          case _ => (matched, foCandidates, ccp :: unmatchedCcps)
        }
    }
    matchingResult
  }

  def matchingOneFoToManyCcp(fos: List[Fo], ccps: List[Ccp]): (List[Match], List[Fo], List[Ccp]) = {
    val acc: (List[Match], Fos, Ccps) = (Nil, Nil, ccps)
    val matchingResult = fos.foldLeft(acc) {
      case ((matched, unmatchedFos, unmatchedCcps), fo) =>
        findMatchingCcps(fo, unmatchedCcps) match {
          case Some((matching, remaining)) => (matching :: matched, unmatchedFos, remaining)
          case _ => (matched, fo :: unmatchedFos, unmatchedCcps)
        }
    }
    matchingResult
  }

  def findMatches(fos: List[Fo], ccps: List[Ccp]): List[Match] = {
//    val fosByInstrument = fos.groupBy(_.instrument)
//    val ccpByInstrument = ccps.groupBy(_.instrument)
    implicit val economicsOrd = new Ordering[Economics] {
      override def compare(x: Economics, y: Economics): Int =
        if (x.quantity != y.quantity)
          x.quantity.compare(y.quantity)
        else
          x.amount.compare(y.amount)
    }
    val sortedFos = fos.sortBy(_.economics)
    val sortedCcps = ccps.sortBy(_.economics)
    val (mergedCcpMatch, remainingFos, remainingCcps) = matchingOneCcpToManyFo(sortedFos, sortedCcps)
    val (mergedFoMatch, remainingFos2, remainingCcps2) = matchingOneFoToManyCcp(remainingFos, remainingCcps)
    val ans = mergedCcpMatch ++ mergedFoMatch
    println("----------")
    println(s"Remaining fo: ${remainingFos2.map(_.economics).combineAll}, ${remainingFos2}")
    println(s"Remaining ccp: ${remainingCcps2.map(_.economics).combineAll}, ${remainingCcps2}")
    println("<")
    ans.foreach(println)
    println("----------")
    ans
  }
}