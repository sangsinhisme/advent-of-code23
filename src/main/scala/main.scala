import scala.io.Source
import scala.util.matching.Regex

object Main {

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("src/main/scala/data_day3.txt")
    val lines =
      try source.getLines().toList
      finally source.close()

    val numberPattern: Regex = "\\d+".r
    val specialPattern: Regex = "\\*".r
    var index, start, end, num = 0
    var listOfFlags: List[Map[Int, (Int, Int)]] = List.empty

    def is_gear(num: Int, index: Int, start: Int, end: Int): Int = {
//      def checkGear(idx: Int): Int = {
//        have_gear_nearby(lines.apply(idx).substring(start, end))
//        if (
//          (lines.apply(idx).substring(start, start + 1).equals("*") ||
//            lines
//              .apply(idx + 1)
//              .substring(start, start + 1)
//              .equals("*")) && lines.length != end
//        ) {
//          val find_pre_gear = s"(\\d+)\\s*\\*$num".r
//          find_pre_gear.findFirstMatchIn(lines.apply(idx)) match {
//            case Some(s) =>
//              s.matched.replace(s"*$num", "").toInt * num
//            case _ =>
//              println(s"${lines.apply(idx)} $num")
//              println(
//                s"${lines.apply(idx).replace(lines.apply(idx).substring(0, start + 1), lines.apply(idx + 1).substring(0, start) + "*")}"
//              )
//              println(s"${lines.apply(idx + 1)} after")
//              println()
//              0
//          }
//
//        } else 0
//      }
      index match {
        case 0 =>
          val current = lines.apply(index).substring(start, end)
          val above = lines.apply(index + 1).substring(start, end)
          specialPattern.findFirstMatchIn(current) match {
            case Some(flag) => println(flag)
            case _          => specialPattern.findFirstMatchIn(above) match
              case Some(flag) =>
                listOfFlags = listOfFlags :+  Map(index -> (flag.start + start, num))
              case _ =>
          }
          println(listOfFlags)
          0
        case lastIndex if lastIndex == lines.length - 1 => 0
        case _                                          => 0
      }
    }

    var result = 0
    for (gear <- lines) {
      numberPattern.findAllMatchIn(gear).foreach { numbersMatch =>
        {
          val index_start = numbersMatch.start
          val index_end = numbersMatch.end
          start = if (index_start - 1 >= 0) index_start - 1 else index_start
          end = if (index_end + 1 < gear.length) index_end + 1 else index_end
          num = numbersMatch.matched.toInt
          result = result + is_gear(num, index, start, end)
        }
      }
      index += 1
    }
    println(result)
  }
}
