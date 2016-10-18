object Factorial extends App {
  def factorial(n: Int): BigInt =
    Cache.cacheGet(n ,
    n match {
      case _ if n <= 1 => 1
      case _ => n * factorial(n-1)
    })

  def natural(n: Int): Stream[Int] = n #:: natural(n+1)

  println(natural(0).map(x => factorial(x)).take(100).toList)
}
