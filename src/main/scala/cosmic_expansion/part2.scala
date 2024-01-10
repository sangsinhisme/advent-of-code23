package cosmic_expansion

import cosmic_expansion.Space
import utils.CommonUtils

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object part2 {

  def main(args: Array[String]): Unit = {

    // Fetch input day 11
    CommonUtils.fetchInput(11,2023)
    val source = CommonUtils.convert2arr[Char](11)
    val expand = Space().space_expands(source)
    val galaxies = Space().find_galaxies(expand)
    val result = galaxies.combinations(2).flatMap { case Array(galaxy1, galaxy2) =>
      Seq(
        Space().steps(galaxy1, galaxy2)
      )
    }.sum

    println(s"My answer: $result")
//    val submit = CommonUtils.submit_answer(11, 2023, answer = result.toString, level = 2)
//    println(submit)
  }
}
