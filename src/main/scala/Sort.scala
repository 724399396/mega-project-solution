object Sort extends App {
  def mergeSort[A](list: List[A])(implicit ev: A => Int): List[A] = {
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

  def bubbleSort[A <% Ordered[A]](list: List[A]): List[A] = {
    import scala.collection.mutable.MutableList
    val mutableList = list.foldLeft(MutableList[A]()) {
      case (acc,x) => acc += x
    }

    for {
      i <- 0 until list.size
    } {
      var min = i
      for {        
        j <- i until list.size
      } {
        if (mutableList(j) < mutableList(min))
          min = j
      }
      val tmp = mutableList(i)
      mutableList(i) = mutableList(min)
      mutableList(min) = tmp
    }

    mutableList.foldRight(List[A]()) {
      case (x,acc) => x :: acc
    }
  }

  println(mergeSort(List(5,12,2,1,3,42,4,5)))
  println(bubbleSort(List(5,12,2,1,3,42,4,5)))
}
