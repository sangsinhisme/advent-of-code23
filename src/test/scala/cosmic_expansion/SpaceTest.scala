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

  def testFindSpaces(input_source: (Int, Int), input_dest: (Int, Int), expectedSpaces: Int): Unit = {
    val galaxies = Space().space_expands_multiple(CommonUtils.convert2arr[Char]("src/test/scala/cosmic_expansion/01.txt"))
    val output = Space().steps(input_source, input_dest, galaxies, 2)
    println(s"$input_source $input_dest $expectedSpaces $output")
    assert(output == expectedSpaces)
  }

  test("find spaces ?") {
    val input = Array(
      ((0, 2), (0, 9), 9),
      ((0, 2), (1, 5), 5),
      ((0, 2), (3, 0), 6),
      ((0, 2), (4, 9), 14),
      ((0, 2), (6, 4), 11),
      ((0, 2), (7, 1), 10),
      ((0, 2), (7, 8), 17),
      ((0, 2), (9, 6), 17),
      ((0, 9), (1, 5), 6),
      ((0, 9), (3, 0), 15),
      ((0, 9), (4, 9), 5),
      ((0, 9), (6, 4), 14),
      ((0, 9), (7, 1), 19),
      ((0, 9), (7, 8), 10),
      ((0, 9), (9, 6), 16),
      ((1, 5), (3, 0), 9),
      ((1, 5), (4, 9), 9),
      ((1, 5), (6, 4), 8),
      ((1, 5), (7, 1), 13),
      ((1, 5), (7, 8), 12),
      ((1, 5), (9, 6), 12),
      ((3, 0), (4, 9), 12),
      ((3, 0), (6, 4), 9),
      ((3, 0), (7, 1), 6),
      ((3, 0), (7, 8), 15),
      ((3, 0), (9, 6), 15),
      ((4, 9), (6, 4), 9),
      ((4, 9), (7, 1), 14),
      ((4, 9), (7, 8), 5),
      ((4, 9), (9, 6), 11),
      ((6, 4), (7, 1), 5),
      ((6, 4), (7, 8), 6),
      ((6, 4), (9, 6), 6),
      ((7, 1), (7, 8), 9),
      ((7, 1), (9, 6), 9),
      ((7, 8), (9, 6), 6)
    )

    for ((input_source, input_dest, spaces) <- input) {
      testFindSpaces(input_source, input_dest, spaces)
    }

  }




