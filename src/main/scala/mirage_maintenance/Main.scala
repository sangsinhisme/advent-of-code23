package mirage_maintenance

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Main {

  def pre_status(state: Array[Long]): Long = {
    var preArr = state
    var combine_result: Long = state.head
    var minus = 0
    while (!preArr.forall(x => x == 0)) {
      preArr = preArr.sliding(2).map {
        case Array(p1, p2) => p2 - p1
      }.toArray
      combine_result -= preArr.head * math.pow(-1, minus % 2).toInt
      minus += 1
    }
    combine_result
  }


  def _main(args: Array[String]): Unit = {
    val source = Source.fromFile("src/main/scala/mirage_maintenance/puzzle_day9.txt")
    val lines =
      try source.getLines().toList
      finally source.close()

    var result: Long = 0
    for (line <- lines) {
      val env = line.split(" ").map(x => x.toLong)
      val pre = pre_status(env)
      result += pre
      println(s"$result $pre")
    }


  }
}
