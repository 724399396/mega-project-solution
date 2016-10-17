object CollatzConjectrue extends App {
  def step(n: Int): List[Int] = n match {
    case 1 => List(1)
    case _ if (n % 2 == 0) => n :: step(n/2)
    case _ => n :: step(n*3+1)
  }

  println(Stream.from(1).map(step).take(10).toList)
}
