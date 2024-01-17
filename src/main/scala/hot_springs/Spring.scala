package hot_springs
import scala.util.matching.Regex

class Spring {
  def is_criteria(eng: String, cond: Array[Int]): Boolean = {
    val regex_agg: Regex = "#+".r
    val map_input = regex_agg.findAllMatchIn(eng).toArray.map(x => x.matched.length)
    map_input sameElements cond
  }

  def f(eng: String, cond: Array[Int], i: Int): Int = {
    if (i == eng.length) {
      if is_criteria(eng, cond) then 1 else 0
    }
    else {
      if eng(i) == '?' then
        f(eng.updated(i, '#'), cond, i + 1) +
        f(eng.updated(i, '.'), cond, i + 1)
      else f(eng, cond, i + 1)
    }
  }
}
