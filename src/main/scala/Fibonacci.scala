object Fibonacci extends App {

  val Array(n) = args

  def fibonacci(n: Int): BigInt =
    Cache.cacheGet(n, 
      n match {
        case x if x < 2 => x.toLong
        case x => fibonacci(x-1) + fibonacci(x-2)
      })

  println(fibonacci(n.toInt))
}
