package point_incidence

class MirrorTest extends munit.FunSuite:
  import point_incidence.Mirror
  import utils.CommonUtils

  test ("is valid mirror ?") {
    assert(1 == 1)
  }

  test ("is same pattern ?") {
    val source =
      CommonUtils.convert2string("src/test/scala/point_incidence/01.txt")
    val test =
        CommonUtils.convert2string("src/test/scala/point_incidence/02.txt")
    assert(Mirror().is_reflection(source, test))
  }


  test ("fix smudge result?") {
    val source =
      CommonUtils.convert2string("src/test/scala/point_incidence/01.txt")
    val result: Long = Mirror().calling_sum(source, Array.empty)
    println(s"My result: $result")
    assert(result == 7)
  }