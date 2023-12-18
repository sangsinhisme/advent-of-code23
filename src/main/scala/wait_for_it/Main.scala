package wait_for_it

import scala.io.Source
import scala.util.matching.Regex

object Main {

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("src/main/scala/wait_for_it/puzzle_day5.txt")
    val lines =
      try source.getLines().toList
      finally source.close()

    val numberRegex: Regex = "\\d+".r
    val time =
      numberRegex
        .findAllMatchIn(lines.head)
        .toList
        .map(x => x.matched)
        .reduceLeft(_ + _)
        .toLong

    val distance =
      numberRegex
        .findAllMatchIn(lines.last)
        .toList
        .map(x => x.matched)
        .reduceLeft(_ + _)
        .toLong
    var result = 0
    val start = distance / time
    for (i <- start until time) {
      if (i <= (time * i - distance) / i) {
        result += 1
      }
    }
    println(result)
  }
}
