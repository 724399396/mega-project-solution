case class Link(start: Point, end: Point)
case class Graph(ps: Set[Point], ls: Set[Link]) {
  import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}
  val adjMap: Map[Point, Set[Link]] =
    ls.foldLeft(MutableMap[Point,MutableSet[Link]]()) {
      case (acc, l@Link(p1, p2)) =>
        val set1 = acc.getOrElse(p1, MutableSet[Link]())
        val set2 = acc.getOrElse(p2, MutableSet[Link]())
        set1 += l
        set2 += l
        acc(p1) = set1
        acc(p2) = set2
        acc
    }.foldLeft(Map[Point, Set[Link]]()) {
      case (acc, (k, v)) =>
        acc + (k->v.foldLeft(Set[Link]())((acc,x) => acc + x))
    }
}

object GraphFromLink extends App {
  import scala.util.Random

  def randomPoint(): Point = Point(Random.nextInt(10), Random.nextInt(10))

  def randomLink(): Link = Link(randomPoint(), randomPoint())

  val links = List.fill(20)(1).map(_ => randomLink())

  def fromLink(links: List[Link]): Graph = {
    val (ps, ls) = links.foldLeft((Set[Point](), Set[Link]())) {
      case ((ps, ls), link@Link(x,y)) =>
        (ps + x + y, ls + link)
    }
    Graph(ps, ls)
  }

  println(fromLink(links))
}
