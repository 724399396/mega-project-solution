object InvertedIndex extends App {
  import scala.io.Source

  val directory = new java.io.File("/home/work/learning/mega_project/src/main/scala")
  val words2File = for {
    file <- directory.listFiles
    if file.isFile
    line <- Source.fromFile(file).getLines
    word <- line.split("\\s+")
  } yield {
    (word, file.getName)
  }

  val map = words2File.foldLeft(Map[String, Set[String]]()) {
    case (acc, (word, fileName)) =>
      acc + (word -> (acc.getOrElse(word, Set[String]()) + fileName))
  }

  var ok = true
  while (ok) {
    println("input your need find word: ")
    val in = readLine()
    ok = in != null
    if (ok) 
      if (map.contains(in))
        println(s"""$in exist at ${map(in).mkString(",")}""")
  }
}
