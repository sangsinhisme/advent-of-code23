package hot_springs

import scala.annotation.tailrec
import scala.io.Source
import utils.CommonUtils
import hot_springs.Spring

/** As you look out at the field of springs, you feel like there are way more springs than the condition records list. When you examine the records, you discover that they were actually folded up this whole time!
  */

object part2 {

  def main(args: Array[String]): Unit = {

    // Fetch input day 12
    CommonUtils.fetchInput(12, 2023)
    val source = CommonUtils.convert2string(
      "/home/sinhns/workspace/freelancer/src/test/scala/hot_springs/01.txt"
    )
    val games: Array[(String, List[Int])] = source.map { line =>
      val Array(x, y) = line.split(" ")
      (x, y.split(",").map(_.toInt).toList)
    }

    val games2: Array[(String, List[Int])] = games.map { case (x, y) =>
      (x + ("?" + x) * 4, List.fill(5)(y).flatten)
    }
//    CommonUtils.submit_answer(12, 2023, answer = ans.toString, level = 2)

  }
}
