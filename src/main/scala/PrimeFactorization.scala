object PrimeFactorization extends App {
  val primes: Stream[Int] = 2 #:: Stream.from(3).filter(i => primes.takeWhile(p => p*p <= i).forall(p => i % p != 0))

  val Array(n) = args
  val num = n.toInt

  println(primes.takeWhile(p => p * p <= num).filter(p => num % p == 0).toList)
}
