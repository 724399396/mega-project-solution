object FlipCoin extends App {
  import scala.util.Random
  val Array(times) = args

  val coins: Stream[Boolean] = Stream.from(1).map(_ => Random.nextBoolean())

  val res = coins.take(times.toInt).toList
  val (heads, tails) = res.partition(identity)

  println(s"heads: ${heads.size}, tails: ${tails.size}")
}
