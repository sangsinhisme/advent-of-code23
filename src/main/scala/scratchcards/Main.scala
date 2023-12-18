package scratchcards
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("src/main/scala/scratchcards/puzzle_day4.txt")
    val lines =
      try source.getLines().toList
      finally source.close()

    val number_regex = "\\d+".r
    var scratchcards: Map[Int, Int] = Map.empty
    var game_index = 1

    def copy_card(
        index: Int,
        number_win_copy: Int
    ): Unit = {

      var copy_win = number_win_copy
      val copy_current = scratchcards.get(index) match
        case Some(number: Int) => number
        case _ => 1
      scratchcards += (index -> copy_current)
      while (copy_win > 0) {
        val game_update = scratchcards.get(index + copy_win) match
          case Some(num: Int) => num + copy_current
          case None => copy_current + 1

        scratchcards += (index + copy_win -> game_update)
        copy_win -= 1
      }
    }

    for (line <- lines) {

      val Array(winning, having) = line.split("\\|")
      val winning_numbers: Array[Int] =
        number_regex
          .findAllMatchIn(winning.split(":").apply(1))
          .map(_.toString.toInt)
          .toArray
      val having_numbers: Array[Int] =
        number_regex.findAllMatchIn(having).map(_.toString.toInt).toArray
      val prizes =
        having_numbers.filter(number => winning_numbers.contains(number))

      copy_card(game_index, prizes.length)
      game_index += 1
    }
    println(scratchcards.foldLeft(0)(_ + _._2))
  }
}
