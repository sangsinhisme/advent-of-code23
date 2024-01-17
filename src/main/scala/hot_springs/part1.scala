package hot_springs

import scala.annotation.tailrec
import scala.io.Source
import utils.CommonUtils
import hot_springs.Spring

object part1 {

  def main(args: Array[String]): Unit = {

    // Fetch input day 11
    CommonUtils.fetchInput(12,2023)
    val source = CommonUtils.convert2string(12)
    val (eng, cond) = source.map {
      line => (line.split(" ")(0), line.split(" ")(1).split(',').map(x => x.toInt))
    }.unzip
    var ans = 0
    for (i <- eng.indices){
      ans += Spring().f(eng(i), cond(i), 0)
    }
    println(s"My answer: $ans")
    CommonUtils.submit_answer(12, 2023, answer = ans.toString, level = 1)

  }
}
