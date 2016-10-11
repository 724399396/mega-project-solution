object FindPi extends App {
  import scala.util.Random
  
  val Array(deep) = args
  val r = new Random()
  val inf: Stream[Int] = 1 #:: inf

  val randPoint = inf.take(deep.toInt).map(_ => (r.nextDouble, r.nextDouble)).filter{ case (x,y) => (Math.pow(x-0.5,2) + Math.pow(y-0.5,2)) <= 0.25}

  val pi = randPoint.size.toDouble / deep.toDouble / 0.25

  println(pi)
}
