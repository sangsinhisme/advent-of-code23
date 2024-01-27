package parabolic_dish

import utils.CommonUtils

import scala.annotation.tailrec
import scala.io.Source

object part2 {

  def main(args: Array[String]): Unit = {

    // Fetch input day 14
    CommonUtils.fetchInput(14, 2023)
    val source =
      CommonUtils.convert2list(14)
    val reverse = source.map(_.toList).transpose.map(_.mkString)
    val result = Dish().call_sum(reverse)
    println(s"My result: $result")
    CommonUtils.submit_answer(14, 2023, answer = result.toString, level = 2)
  }
}
