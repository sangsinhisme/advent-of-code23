package cosmic_expansion

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex
import utils.CommonUtils
import cosmic_expansion.Space

object part1 {

  def main(args: Array[String]): Unit = {

    // Fetch input day 11
    CommonUtils.fetchInput(11,2023)
    val source = CommonUtils.convert2arr[Char]("src/test/scala/cosmic_expansion/01.txt")
    val expand = Space().space_expands(source)
    val galaxies = Space().find_galaxies(expand)

    val result = galaxies.combinations(2).flatMap { case Array(galaxy1, galaxy2) =>
      Seq(
        Space().steps(galaxy1, galaxy2)
      )
    }.sum

    println(result)
    CommonUtils.submit_answer(11, 2023, answer = result.toString, level = 1)
  }
}
