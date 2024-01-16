package hot_springs

import scala.annotation.tailrec
import scala.io.Source
import utils.CommonUtils

object part1 {

  def main(args: Array[String]): Unit = {

    // Fetch input day 11
    CommonUtils.fetchInput(12,2023)
    val source = CommonUtils.convert2string(12)
    for (line <- source){
      println(line)
    }
  }
}
