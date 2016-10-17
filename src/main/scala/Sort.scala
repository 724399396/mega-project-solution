object Sort extends App {
  def mergeSort[A <: Ordered[A]](list: List[A]): List[A] = {
    def merge(a: List[A], b: List[A]): List[A] = {
      (a,b) match {
        case (List(), _) => b
        case (_, List()) => a
        case (x::xs, y::ys) =>
          if (x<y) x :: merge(xs, b)
          else y :: merge(a, ys)
      }
    }

    list match {
      case List() => List()
      case List(x) => List(x)
      case x =>
        val (left, right) = list.splitAt(x.size/2)
        val sortedLeft = mergeSort(left)
        val sortedRight = mergeSort(right)
        merge(sortedLeft, sortedRight)
    }

  }
}
