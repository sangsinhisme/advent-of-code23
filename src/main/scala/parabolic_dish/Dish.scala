package parabolic_dish

class Dish {
  def load_edge(rock: List[Int], immute: List[Int], size: Int): Int = {
    var minus: Int = -1
    var init_shaped: Int = 0
    (1 to rock.length).flatMap { i =>
      val current_shaped = immute(immute.lastIndexWhere(x => x <= rock(i - 1)))
      if (current_shaped != init_shaped) {
        minus = -1
        init_shaped = current_shaped
      }
      minus += 1
      Seq(size - current_shaped - minus)
    }.sum
  }
  def sumOfRocks(input: String): Int = {
    val immute: List[Int] = input.zipWithIndex
      .filter { case (char: Char, _) =>
        char == '#'
      }
      .map { case (_, index: Int) =>
        index
      }
      .toList
    val rocks: List[Int] = input.zipWithIndex
      .filter { case (char: Char, _) =>
        char == 'O'
      }
      .map { case (_, index: Int) =>
        index
      }
      .toList
    load_edge(
      rocks.map(x => x + 1),
      List(0) ++ immute.map(x => x + 1),
      input.length
    )
  }

  def call_sum(input: List[String]): Int = {
    if (input.isEmpty) 0
    else sumOfRocks(input.head) + call_sum(input.tail)
  }
}
