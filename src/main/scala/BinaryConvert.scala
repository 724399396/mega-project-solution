object BinaryConvert extends App {
  var Array(input) = args

  def convert2Binary(decimal: String): String = {
    def help(decimal: Int):String =
      decimal match {
        case 0 => "0"
        case x => "" + x%2 + help(x/2)
      }
    help(decimal.toInt).reverse.dropWhile(_ == '0')
  }

  def convert2Decimal(binary: String): String = {
    def help(binary: List[Char]): Int = {
      binary match {
        case List() => 0
        case x :: xs => 2 * help(xs) + x.toString.toInt
      }
    }
    help(binary.reverse.toList).toString
  }

  println(s"input: $input, binary:${convert2Binary(input)}, decimal: ${convert2Decimal(convert2Binary(input))}")
}
