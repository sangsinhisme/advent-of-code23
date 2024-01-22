package point_incidence

class MirrorTest extends munit.FunSuite:
  import point_incidence.Mirror
  import utils.CommonUtils

  test ("is valid mirror?") {
    assert(1 == 1)
  }

  test ("is same pattern") {
    val source =
      CommonUtils.convert2string("src/test/scala/point_incidence/01.txt")
    val result: Int = Mirror().calling_sum(source, Array.empty)
    println(result)
    assert(result == 405)
  }