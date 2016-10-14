object FindE extends App {
  
  val Array(deep) = args
  def naturalNum(x: Int): Stream[Int] = 
    x #:: naturalNum(x+1)

  def factorial(n: Int): Long =
    Cache.cacheGetLong(n,
      n match {
        case x if x <= 2 => x.toLong
        case x => x * factorial(x-1)
      })

  val e = naturalNum(1).map(x => 1.0.toDouble/factorial(x)).take(deep.toInt).filter(!_.isInfinite).sum + 1
  println(e)
}
