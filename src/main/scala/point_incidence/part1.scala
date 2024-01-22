package point_incidence

import hot_springs.Spring
import utils.CommonUtils

import scala.annotation.tailrec
import scala.io.Source
import point_incidence.Mirror

object part1 {

  def main(args: Array[String]): Unit = {

    // Fetch input day 13
    CommonUtils.fetchInput(13, 2023)
    val source =
      CommonUtils.convert2string("src/test/scala/point_incidence/01.txt")
    val result: Long = Mirror().calling_sum(source, Array.empty)
    println(s"My result: $result")
  }
}
