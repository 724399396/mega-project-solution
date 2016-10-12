object Cache {
  import scala.collection.mutable.{ Map => MutableMap }
  val cacheMap: MutableMap[Int, Long] = MutableMap()

  def cacheGet(n: Int, f: => Long) = {
    lazy val res = f
    cacheMap.get(n) match {
      case None => cacheMap(n) = res; res
      case Some(x) => x
    }
  }
}
