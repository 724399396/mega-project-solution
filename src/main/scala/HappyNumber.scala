object HappyNumber extends App {
  val postives = Stream.from(1)

  def numSeq(n: Int): Stream[Int] = n #:: numSeq(n.toString.map(x => Math.sqrt(x.toString.toDouble).toInt).sum)

  println(postives.map(x => numSeq(x).take(100)).filter(_.contains(1)).take(8).map(_.head).toList)
}
