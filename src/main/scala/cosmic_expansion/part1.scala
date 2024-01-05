package cosmic_expansion

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex
import utils.CommonUtils

object part1 {

  def main(args: Array[String]): Unit = {

    // Fetch input day 11
    CommonUtils.fetchInput(9,2023)
    val source = CommonUtils.convert2arr[Char](11, 'a')
    for (i <- source) {
      println(i.mkString("Array(", ", ", ")"))
    }
  }
}
