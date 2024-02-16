package clumsy_crucible

import utils.CommonUtils


object part1 {

  def main(args: Array[String]): Unit = {

    // Fetch input day 17
    CommonUtils.fetchInput(17,2023)
    val source = CommonUtils.convert2list(17).map(x => x.toList)
    val result = source.length

    println(s"My answer: $result")
//    CommonUtils.submit_answer(16, 2023, answer = result.toString, level = 1)
  }
}
