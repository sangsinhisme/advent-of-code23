package lens_library

import utils.CommonUtils
import lens_library.Hash

object part1 {

  def main(args: Array[String]): Unit = {

    // Fetch input day 15
    CommonUtils.fetchInput(15,2023)
    val source = CommonUtils.convert2string(15)
    val sequence = source.mkString.split(",")
    val ans = (for {
      word <- sequence
    } yield {
      Hash().hash_to_ascii(word)
    }).sum

    println(s"My answer: $ans")
    CommonUtils.submit_answer(15, 2023, answer = ans.toString, level = 1)
  }
}
