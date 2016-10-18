case class Link(start: Point, end: Point)
case class Graph(ps: Set[Point], ls: Set[Link])

object GraphFromLink extends App {
  import scala.util.Random

  def randomPoint(): Point = Point(Random.nextInt(10), Random.nextInt(10))

  def randomLink(): Link = Link(randomPoint(), randomPoint())

  val links = List.fill(20)(1).map(_ => randomLink())

  val (ps, ls) = links.foldLeft((Set[Point](), Set[Link]())) {
    case ((ps, ls), link@Link(x,y)) =>
      (ps + x + y, ls + link)
  }

  println(Graph(ps, ls))
}
