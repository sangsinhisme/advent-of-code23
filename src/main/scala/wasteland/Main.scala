package wasteland

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Main {

  @tailrec
  def gcd(a: Long, b: Long): Long = if (b==0) a else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = {
    (a * b).abs / gcd(a, b)
  }

  def _main(args: Array[String]): Unit = {
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

    val visitNode = document.filter {
      x => x._1.last == 'A'
    }.keys.toArray

    var result: Long = 1
    for (node <- visitNode) {
      var step: Long = 0
      var newNode = node
      var shouldBreak = false
      while (!newNode.endsWith("Z") && !shouldBreak) {
        for (navigate <- instruction) {
          newNode = document.get(newNode) match
            case Some((left, right)) =>
              navigate match
                case 'L' => left
                case 'R' => right
            case _ => newNode
          if newNode.endsWith("Z") then {
            step += 1
            shouldBreak = true
          }
          if !shouldBreak then step +=1
        }
      }
      result = lcm(step, result)
      println(s"$node $newNode $step $result")

    }
    println(result)
  }
}
