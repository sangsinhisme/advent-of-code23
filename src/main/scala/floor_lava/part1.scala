package floor_lava

import utils.CommonUtils


object part1 {

  def main(args: Array[String]): Unit = {

    // Fetch input day 16
    CommonUtils.fetchInput(16,2023)
    val source = CommonUtils.convert2list(16).map(x => x.toList)
    val result = Lava().heat_mirror(source)
    println(result)
    CommonUtils.submit_answer(16, 2023, answer = result.toString, level = 1)
  }
}
