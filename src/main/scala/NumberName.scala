object NumberName extends App {
  val num = "1232432"

  val unitMap = Map(0 -> "", 1 -> "thounds", 2 -> "millions", 3 -> "billions")
  val nameMap = Map(0 -> "zero", 1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine")


  val res = for {
    (str, i) <- num.reverse.grouped(3).map(_.reverse).toList.zipWithIndex
  } yield str.map(x => nameMap(x.toString.toInt)).mkString(" ") + " " + unitMap(i)

  println(res.reverse.mkString(" "))
}
