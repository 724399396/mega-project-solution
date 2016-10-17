object FastExponentiation extends App {
  val Array(a,b) = args

  def pow(a: Double, b: Double): Double = {
    if (b < 2.0)
      Math.pow(a,b)
    else {
      val one = pow(a, b/2)
      one * one
    }
  }

  println(pow(a.toDouble, b.toDouble))
}
