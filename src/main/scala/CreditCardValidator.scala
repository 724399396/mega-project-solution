object CreditCardValidator extends App {
  val Array(numStr) = args

  val num = numStr.map(_.toString.toInt)
  val x = num.reverse.head
  val left = num.reverse.tail
  val sumDigits = (for {
    (digit, index) <- left.zipWithIndex    
  } yield 
    if (index % 2 == 0) {
      val doubleIt = digit * 2
      if (doubleIt > 9)
        (doubleIt - 9)
      else
        doubleIt
    } else 
      digit )
  println(sumDigits.sum * 9 % 10 == x)
}
