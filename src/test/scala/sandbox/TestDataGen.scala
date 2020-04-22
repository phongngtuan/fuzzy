package sandbox
import cats.implicits._

object TestDataGen {

  val ccps = LazyList.from(1).map(id => Ccp(id.toString, "", Economics(id, id*2)))
  def testData(mergeFactor: Int, n: Int) = {
    val fo = Fo("fo", "", ccps.slice(n / 2, n / 2 + mergeFactor).map(_.economics).combineAll)
    (fo, ccps.take(n).toList)
  }

}
