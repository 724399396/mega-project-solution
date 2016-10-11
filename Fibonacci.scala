object Fibonacci extends App {

  val Array(n) = args

  def fibonacci(n: Int): Long =
    Cache.cacheGet(n, 
      n match {
        case x if x <= 2 => 1
        case x => fibonacci(x-1) + fibonacci(x-2)
      })

  println(fibonacci(n.toInt))
}
