object SieveOfEratosthenes extends App {
  val primes: Stream[Int] = 2 #:: Stream.from(3).filter(i => primes.takeWhile(p => p * p <= i).forall(p => i % p != 0))

  println(primes.take(100).toList)
}
