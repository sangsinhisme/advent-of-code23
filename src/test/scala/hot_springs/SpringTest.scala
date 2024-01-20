package hot_springs
import scala.annotation.tailrec
import scala.util.matching.Regex

class SpringTest extends munit.FunSuite:

  import hot_springs.Spring
  import hot_springs.SpringTrue
  import utils.CommonUtils
  test ("is valid arrangements?") {
    assert(Spring().is_criteria("#.#.###.#.#.###.#.#.###.#.#.###.#.#.###", Array(1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 3)))
  }

  test ("is valid arrangements 2 ?") {
    val input = CommonUtils.convert2string("src/test/scala/hot_springs/01.txt")
    val (eng, cond) = input.map {
      line => (line.split(" ")(0), line.split(" ")(1).split(',').map(x => x.toInt))
    }.unzip
    val test_case = Array(1, 4, 1, 1, 4, 10)
    for (i <- eng.indices){
      assert(Spring().f(eng(i), cond(i), 0) == test_case(i))
    }
  }

  test ("multiple valid arrangements 3 ?"){
    val input = CommonUtils.convert2string("src/test/scala/hot_springs/01.txt")
    val (eng, cond) = input.map {
      line => (line.split(" ")(0), line.split(" ")(1).split(',').map(x => x.toInt))
    }.unzip
    val test_case = Array(1, 16384, 1, 16, 2500, 506250)
    for (i <- eng.indices){
      assert(Spring().f(eng(i), cond(i), 0) == test_case(i))
    }
  }

  test ("f2 update ?") {
    CommonUtils.fetchInput(12, 2023)
    val source = CommonUtils.convert2string(12)
    var ans: Long = 0
    for (line <- source){
      ans += SpringTrue().sumOfUnfoldedSpringConfigurations(line)
    }
    println(ans)

  }



