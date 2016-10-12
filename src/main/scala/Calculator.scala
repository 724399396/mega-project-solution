object Calculator extends App {
  import scala.collection.mutable.Stack
  import scala.io.StdIn
  
  val line = StdIn.readLine()
  val stack = Stack[Double]()

  line.split("\\s+").foreach { 
    case "+" => 
      stack.push(stack.pop() + stack.pop())
    case "*" =>
      stack.push(stack.pop() * stack.pop())
    case "-" =>
      val snd = stack.pop()
      stack.push(stack.pop() - snd)
    case "/" =>
      val snd = stack.pop()
      stack.push(stack.pop() / snd)
  }
}
