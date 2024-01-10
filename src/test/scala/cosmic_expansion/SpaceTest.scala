package cosmic_expansion

class SpaceTest extends munit.FunSuite:

  import cosmic_expansion.Space
  import utils.CommonUtils
  test("space expand correct ?") {
    val input = CommonUtils.convert2arr[Char]("src/test/scala/cosmic_expansion/01.txt")
    val output = CommonUtils.convert2arr[Char]("src/test/scala/cosmic_expansion/02.txt")
    val output_function = Space().space_expands(input)

    for (i <- output.indices){
      assert(output_function(i) sameElements output(i))
    }
  }

  test("find galaxies ?") {
    val input = CommonUtils.convert2arr[Char]("src/test/scala/cosmic_expansion/02.txt")
    val output = Array((4, 0), (9, 1), (0, 2), (8 ,5), (1, 6), (12, 7), (9, 10), (0, 11), (5, 11))
    val output_function = Space().find_galaxies(input)
    assert(output.sorted sameElements output_function.sorted)

  }



