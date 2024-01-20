package hot_springs
import scala.annotation.tailrec
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

  def countPoss(g: (String, List[Int])): Int = {
    val (pos, hints) = g
    val p: Regex = "#+\\.".r
    val regex_agg: Regex = "#+".r
    if (!pos.contains("?")) {
      if (regex_agg.findAllMatchIn(pos).map(x => x.matched.length).sum == hints.sum) 1 else 0
    } else {
      val i = pos.indexOf("?")
      val y = p.findAllMatchIn(pos.take(i + 1)).map(m => (m.start, m.end)).toList
      val x = y.map { case (start, end) => end - start - 1 }

      if (hints.length < x.length || x != hints.take(x.length) || y.nonEmpty && y.last._2 != i) {
        0
      } else {
        val j = if (y.isEmpty) 0 else y.last._2
        val nh = hints.drop(x.length)
        countPoss((pos.patch(j, nh.mkString, 0).stripSuffix("."), nh)) + countPoss((pos.patch(j, "".mkString, 1), nh))
      }
    }
  }

}
