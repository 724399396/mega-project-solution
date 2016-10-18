// This Solution not working, I'm trying
object EulerianPath extends App {
  import scala.collection.mutable.{Stack, MutableList}

  def eulerianPath(input: Graph): Either[String, List[Link]] = {
    println(input.adjMap)
    def even(i: Int): Boolean = i % 2 == 0
    def odd(i: Int): Boolean = !even(i)
    def otherP(l: Link, p: Point): Point = {
      val Link(p1, p2) = l
      if (p1 == p) p2 else p1
    }

    val oddPointNum = input.adjMap.count { case (_, adjLink) => odd(adjLink.size) }
    if (oddPointNum != 0 && oddPointNum != 2)
      Left(s"exit $oddPointNum odd link num point, eulerian path not exist")
    else {
      val point2UnusedLink = input.adjMap.map {
        case (p, links) =>
          (p, links.foldLeft(Stack[Link]())(_ :+ _))
      }

      var finalPath = MutableList[Link]()
      var workPoint = Set[Point]()
      while (finalPath.size != input.ls.size) {
        val Link(p1, p2) = point2UnusedLink.values.flatten.toList.head
        val start = if (workPoint.contains(p1)) p2 else p1
        var current = start
        val path = MutableList[Link]()
        while (path.isEmpty || start != current) {
          workPoint += current
          val unsedLinks = point2UnusedLink(current)
          val nL = unsedLinks.pop
          path += nL
          current = otherP(nL, current)
        }
        workPoint += current
        val (path1, path2) = finalPath.span { case Link(bp1, bp2) => bp2 == start }
        finalPath = path1 ++ path ++ path2
      }

      Right(finalPath.foldRight(List[Link]())(_ :: _))
    }
  }

  println(eulerianPath(GraphFromLink.fromLink(List(Link(Point(1,1), Point(2,2)), Link(Point(1,1), Point(3,3)), Link(Point(2,2), Point(3,3))))))
}
