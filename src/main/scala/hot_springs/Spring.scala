package hot_springs
import scala.util.matching.Regex

class Spring {
  def is_criteria(engineers: String, condition: Array[Int]): Boolean = {
    val regex_agg: Regex = "#+".r
    val map_input = regex_agg.findAllMatchIn(engineers).toArray.map(x => x.matched.length)
    map_input sameElements condition
  }
}
