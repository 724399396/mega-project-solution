object Cache {
  import scala.collection.mutable.{ Map => MutableMap }
  val cacheMap: MutableMap[Int, BigInt] = MutableMap()
  val cacheMap2: MutableMap[Int, Long] = MutableMap()

  def cacheGet(n: Int, f: => BigInt) = {
    lazy val res = f
    cacheMap.get(n) match {
      case None => cacheMap(n) = res; res
      case Some(x) => x
    }
  }

  def cacheGetLong(n: Int, f: => Long) = {
    lazy val res = f
    cacheMap2.get(n) match {
      case None => cacheMap(n) = res; res
      case Some(x) => x
    }
  }

}
