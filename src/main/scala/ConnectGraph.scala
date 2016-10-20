object ConnectedGraph extends App {

  def connect(input: Graph): Boolean = {
    val Graph(ps, ls) = input
    ls.foldLeft(Set[Point]()) {
      case (acc, Link(p1,p2)) => acc + p1 + p2
    } == ps
  }

  println(connect(GraphFromLink.fromLink(List(Link(Point(1,1), Point(2,2)), Link(Point(1,1), Point(3,3)), Link(Point(2,2), Point(3,3))))))
}
