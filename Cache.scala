object Cache {
  import scala.collection.mutable.{ Map => MutableMap }
  val cacheMap: MutableMap[Int, Long] = MutableMap()

  def cacheGet(n: Int, f: => Long) =
    cacheMap.get(n) match {
      case None => cacheMap(n) = f; f
      case Some(x) => x
    }
}
