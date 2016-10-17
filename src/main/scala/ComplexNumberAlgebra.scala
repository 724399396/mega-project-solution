case class ComplexNumber(a: Double, b: Double) {
  def +(other: ComplexNumber): ComplexNumber = {
    ComplexNumber(this.a + other.a, this.b + other.b)
  }

  def *(other: ComplexNumber): ComplexNumber = {
    ComplexNumber(this.a*other.a-this.b*other.b, (b*other.a + a*other.b))
  }

  def /(other: ComplexNumber): ComplexNumber = {
    val ComplexNumber(c,d) = other
    val denominator = c*c + d*d
    ComplexNumber((a*c+b*d)/denominator, (b*c-a*d)/denominator)
  }

  override def toString: String = s"$a + ${b}i"
}

object ComplexNumberAlgebra extends App {
  println(ComplexNumber(3, 4) + ComplexNumber(5,6))
  println(ComplexNumber(3, 4) * ComplexNumber(5,6))
  println(ComplexNumber(3, 4) / ComplexNumber(5,6))
}
