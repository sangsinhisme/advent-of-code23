package lens_library

class HashTest extends munit.FunSuite:
  import utils.CommonUtils
  import lens_library.Hash

  test ("is hash working ?") {
    val input = "HASH-"
    val true_ground = List(72, 65, 83, 72, 45)
    val test = input.toList.map(char => char.toByte)
    assert(test == true_ground)

    val input_hash = "rn=1"
    val decode = 30
    assert(Hash().hash_to_ascii(input_hash) == decode)
  }

  test("test hash algorithm ?") {
    val source = CommonUtils.convert2list("src/test/scala/lens_library/01.txt")
    val sequence = source.mkString.split(",")
    val ans = (for {
      word <- sequence
    } yield {
      Hash().hash_to_ascii(word)
    }).sum
    assert(ans == 1320)
  }
