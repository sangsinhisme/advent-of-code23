package wasteland

import scala.io.Source
import scala.util.matching.Regex

object Main {

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("src/main/scala/wasteland/puzzle_day8.txt")
    val lines =
      try source.getLines().toList
      finally source.close()

    val instruction: Array[Char] = lines.head.toArray
    val elementRegex: Regex = "\\w+".r
    var document: Map[String, (String, String)] = Map.empty

    for (line <- lines.tail) {
      if (line.nonEmpty) {
        val Array(elem1, elem2, elem3) = elementRegex.findAllMatchIn(line).map(x => x.matched).toArray
        document = document.updated(elem1, (elem2, elem3))
      }
    }

    var visitNode = document.filter {
      x => x._1.last == 'A'
    }.keys.toArray

    var step = 0
    var loop = 0
    while (visitNode.count(x => x.last == 'Z') != visitNode.length) {
      if (step > instruction.length) {
        val navigate = instruction.apply(step)
        step += 1
      }
      else {
        step = 0
        loop += 1
      }
    }

//    var visited: Array[String] = Array("AAA")
//    while (visited.last != "ZZZ") {
//      for (navigate <- instruction) {
//        document.get(visited.last) match {
//          case Some((left, right)) =>
//            navigate match
//              case 'L' => visited = visited :+ left
//              case 'R' => visited = visited :+ right
//
//          case None =>
//        }
//      }
//    }
//
//    println(visited.length - 1)
  }
}
