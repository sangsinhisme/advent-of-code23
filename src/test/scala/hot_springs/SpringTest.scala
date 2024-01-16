package hot_springs

class SpringTest extends munit.FunSuite:

  import hot_springs.Spring
  import utils.CommonUtils
  test ("is valid arrangements?") {
    assert(Spring().is_criteria("#.#.###", Array(1, 1, 3)))
  }




