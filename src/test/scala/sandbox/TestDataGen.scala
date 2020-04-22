package sandbox
import cats.implicits._

object TestDataGen {

  val ccps = LazyList.from(1).map(id => Ccp(id.toString, "", Economics(id, id*2)))
  val fos = LazyList.from(1).map(id => Fo(id.toString, "", Economics(id, id*2)))

  def splitCcps(mergeFactor: Int, n: Int): (Fo, List[Ccp]) = {
    val fo = Fo("fo", "", ccps.slice(n / 2, n / 2 + mergeFactor).map(_.economics).combineAll)
    (fo, ccps.take(n).toList)
  }

  def splitFos(mergeFactor: Int, n: Int): (Ccp, List[Fo]) = {
    val ccp = Ccp("fo", "", fos.slice(n / 2, n / 2 + mergeFactor).map(_.economics).combineAll)
    (ccp, fos.take(n).toList)
  }
}
