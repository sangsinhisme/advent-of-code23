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

  test("focusing power ?") {
    val hash = new Hash()
    var boxes = List(
      hash.Box(0, List.empty)
    )
    val source = CommonUtils.convert2list("src/test/scala/lens_library/01.txt")
    val sequence = source.mkString.split(",")
    for (word <- sequence){
      boxes = hash.lens_hashmap(word, boxes)
    }
    val result = (for {
      box <- boxes
    } yield {
      val index = box.index
      val lens = box.lens
      (for {
        len <- lens
      } yield {
        (index + 1) * (lens.indexWhere(x => x == len) + 1) * len.length
      }).sum
    }).sum
    assert(result == 145)
  }
