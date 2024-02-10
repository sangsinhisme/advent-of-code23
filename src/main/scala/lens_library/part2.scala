package lens_library

import lens_library.Hash
import utils.CommonUtils

object part2 {

  def main(args: Array[String]): Unit = {

    // Fetch input day 15
    CommonUtils.fetchInput(15,2023)
    val source = CommonUtils.convert2string(15)
    val sequence = source.mkString.split(",")
    val hash = new Hash()
    var boxes = List(
      hash.Box(0, List.empty)
    )
    for (word <- sequence){
      boxes = hash.lens_hashmap(word, boxes)
    }
    val ans = (for {
      box <- boxes
    } yield {
      val index = box.index
      val lens = box.lens
      (for {
        len <- lens
      } yield {
        (index + 1) * (lens.indexWhere(x => x == len) + 1) * len.length
      }).sum
    }).sum
    println(s"My answer: $ans")
    CommonUtils.submit_answer(15, 2023, answer = ans.toString, level = 2)
  }
}
