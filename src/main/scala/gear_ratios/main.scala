import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex

object main {

  def main_gear_ratios(args: Array[String]): Unit = {
    val source = Source.fromFile("src/main/scala/data_day3.txt")
    val lines =
      try source.getLines().toList
      finally source.close()

    val specialPattern: Regex = "\\*".r
    var index = 0

    def find_number(index: Int, gear: String, switch: String): String = {
      var number = ""
      var _loop = index
      switch match
        case "left" =>
          while (_loop >= 0 && gear.apply(_loop).isDigit) {
            number = gear.apply(_loop) + number
            _loop = _loop - 1
          }
        case "right" =>
          while (_loop <= lines.length - 1 && gear.apply(_loop).isDigit) {
            number = number + gear.apply(_loop)
            _loop += 1
          }
      number
    }

    def check_left_right(index_check: Int, gear: String, list: ListBuffer[Int]): ListBuffer[Int] = {
      if gear.apply(index_check - 1).isDigit then {
        list.append(find_number(index_check - 1, gear, "left").toInt)
      }
      if gear.apply(index_check + 1).isDigit then {
        list.append(find_number(index_check + 1, gear, "right").toInt)
      }
      list
    }

    var result = 0
    for (gear <- lines) {
      specialPattern.findAllMatchIn(gear).foreach { numbersMatch =>
        {
          val index_gear = numbersMatch.start
          var list_gear_number: ListBuffer[Int] = ListBuffer()
          index match {
            case _ if index_gear != 0 && index_gear != lines.length - 2 =>
              list_gear_number = check_left_right(index_gear, gear, list_gear_number)
              if lines.apply(index + 1).apply(index_gear).isDigit then {
                val number_left = find_number(index_gear, lines.apply(index + 1), "left")
                val number_right = find_number(index_gear + 1, lines.apply(index + 1), "right")
                list_gear_number.append((number_left + number_right).toInt)
              }
              else {
                list_gear_number = check_left_right(index_gear, lines.apply(index + 1), list_gear_number)
              }
              if lines.apply(index - 1).apply(index_gear).isDigit then {
                val number_left = find_number(index_gear, lines.apply(index - 1), "left")
                val number_right = find_number(index_gear + 1, lines.apply(index - 1), "right")
                list_gear_number.append((number_left + number_right).toInt)
              }
              else {
                list_gear_number = check_left_right(index_gear, lines.apply(index - 1), list_gear_number)
              }
          }
          if (list_gear_number.length == 2) {
            println(list_gear_number)
            result = result + list_gear_number.head * list_gear_number.apply(1)
          }
        }
      }
      index = index + 1
    }
    println(result)
  }
}
