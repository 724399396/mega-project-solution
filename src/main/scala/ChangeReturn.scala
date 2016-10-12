object ChangeReturn extends App {
  val coins = List(1, 2, 5)

  val Array(costStr, payStr) = args

  val shouldReturn: Int = payStr.toInt - costStr.toInt

  def changeMethod(money: Int, coins: List[Int]): List[List[Int]] = 
    (money, coins) match {
      case (0, List()) => List(List())
      case (x, List()) => List()
      case (x, c :: lc) => 
        if (x < c)
          changeMethod(x, lc)
        else
          changeMethod(x - c, coins).map(c :: _) ++ changeMethod(x, lc)
    }

  println(changeMethod(shouldReturn, coins))
}
