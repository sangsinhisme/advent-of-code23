package parabolic_dish

class DishTest extends munit.FunSuite:
  import utils.CommonUtils
  import parabolic_dish.Dish
  import scala.util.control.Breaks._


  test ("is valid ?") {
    assert(1 == 1)
  }

  test ("sum rocks ?") {
    val source =
      CommonUtils.convert2list("src/test/scala/parabolic_dish/01.txt")
    val reverse = source.map(_.toList).transpose.map(_.mkString)
    val result = Dish().call_sum(reverse)
    assert(result == 136)
  }

  test("unit test rotate ?"){
    val immute = List(0, 6)
    val rocks = List(2, 7, 10)
    val result = "O....#OO.."
    val test = Dish().rotate(rocks, immute, result.length)
    assert(test == result)
  }

  test("test full function ?"){
    val source =
      CommonUtils.convert2list("src/test/scala/parabolic_dish/01.txt")
    val reverse = CommonUtils.transpose(source)
    val result = reverse.map(x => Dish().tilt(x))
    val true_rotate = List(
      "OOOO.#.O..", "OO..#....#",
      "OO..O##..O", "O..#.OO...",
      "........#.", "..#....#.#",
      "..O..#.O.O", "..O.......",
      "#....###..", "#....#...."
    )
    assert(CommonUtils.transpose(result) == true_rotate)
  }


  test("deja vu cycle ?"){
    val source: List[String] =
      CommonUtils.convert2list(14)

    var deja_vu: List[(Int, Int)] = List.empty
    breakable{
      for (i <- 1 to 1000) {
        val result = Dish().total_load(source, i)
        val stepBefore = deja_vu.find(pair => pair._2 == result).map(_._1)
        if (Dish().deja_vu(deja_vu, (i, result))) {
          println(s"Already seen at $i: $result")
          stepBefore match
            case Some(x) =>
              println(s"Step Travel: ${(i - x) / 3}")
              val travel_step = (i - x) / 3
              val travelled = i
              val left = (1000000000 - travelled) % travel_step
              println(s"Final Travel: ${deja_vu(x + left - 2)._2}")
            case _ =>
          break
        }
        deja_vu = deja_vu :+ (i, result)
        println(s"current $i: $result")
      }
    }
  }

//  test ("rotate one loop?") {
//    val source = CommonUtils.convert2list("src/test/scala/parabolic_dish/01.txt")
//    val reverse = source.map(_.toList).transpose.map(_.mkString)
//    val result = Dish().call_state_end(reverse, 1)
//    println(result)
//    assert(result == 87)
//    val result_3 = Dish().call_state_end(reverse, 3)
//    assert(result_3 == 69)
//  }


