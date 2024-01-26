package parabolic_dish

class DishTest extends munit.FunSuite:
  import utils.CommonUtils
  import parabolic_dish.Dish
  test ("is valid ?") {
    assert(1 == 1)
  }

  test ("sum rocks ?") {
    val source =
      CommonUtils.convert2list("src/test/scala/parabolic_dish/01.txt")
    val reverse = source.map(_.toList).transpose.map(_.mkString)
    val result = Dish().call_sum(reverse)
    println(result)
    assert(result == 136)
  }
