case class Point(x: Int, y: Int) extends Ordered[Point] {
  override def compare(other: Point) = {
    val xComp = x.compare(other.x)
    if (xComp == 0)
      y.compare(other.y)
    else
      xComp
  }

  def distance(other: Point): Double = {
    val Point(ox, oy) = other
    Math.sqrt(Math.pow(x - ox, 2) + Math.pow(y - oy, 2))
  }
}

object ClosestPair extends App {
  import scala.util.Random

  val testPoints = List.fill(1000)(1).map(_ => Point(Random.nextInt(100), Random.nextInt(100))).toSet.toList

  // O(n^2) method
  def method1(testPoints: List[Point]): (Point, Point, Double) = {
    var closestDis = Double.MaxValue
    var point1 = testPoints(0)
    var point2 = testPoints(0)
    for {
      i <- 0 until testPoints.size
      j <- 0 until testPoints.size
      if i != j
      one = testPoints(i)
      other = testPoints(j)
    } {
      val dis = one.distance(other)
      if (dis < closestDis) {
        closestDis = dis
        point1 = one
        point2 = other
      }
    }
    (point1, point2, closestDis)
  }

  // O(nLgn) method
  def method2(testPoints: List[Point]): (Point, Point, Double) = {
    val sortedPoints = testPoints.sorted
    sortedPoints.init.zip(sortedPoints.tail).map {
      case (one, other) => (one, other, one.distance(other))
    }.minBy { case (_, _, dis) => dis }
  }

  def time[A](a : => A): A = {
    val start = System.currentTimeMillis
    val res = a
    println("elphase time: " + (System.currentTimeMillis() - start))
    res
  }

  println("method1 res: " + time(method1(testPoints)))
  println("method2 res: " + time(method2(testPoints)))
}
