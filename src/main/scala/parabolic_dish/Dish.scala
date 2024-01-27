package parabolic_dish
import utils.CommonUtils

import scala.collection.mutable.ListBuffer

class Dish {

  def rotate(rock: List[Int], immute: List[Int], size: Int): String = {
    val result: ListBuffer[Char] = ListBuffer.fill(size)('.')
    immute.foreach {
      i => if(i > 0) result(i - 1) = '#'
    }
    var init_shaped: Int = 0
    var pos: Int = 0
    for (i <- rock.indices){
      val current_shaped = immute(immute.lastIndexWhere(x => x <= rock(i)))
      if (current_shaped != init_shaped) {
        pos = 0
        init_shaped = current_shaped
      }
      result(init_shaped + pos) = 'O'
      pos += 1
    }
    result.mkString
  }

  def tilt(input: String): String = {
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
    rotate(
      rocks.map(x => x + 1),
      List(0) ++ immute.map(x => x + 1),
      input.length
    )
  }

  def sum_load(source: List[String]): Int = {
    val length = source.length
    source.zipWithIndex.map {
      case (input, index) =>
        (length - index) * input.count(x => x == 'O')
    }.sum
  }

  def total_load(source: List[String], cycles: Int = 1): Int = {
    val result = (1 to cycles).foldLeft(source) { (currentSource, _) =>
      val north = CommonUtils.transpose(currentSource).map(x => Dish().tilt(x)) //north -> west (first start with west 1 transpose)
      val west = CommonUtils.transpose(north).map(x => Dish().tilt(x)) //north -> west (1 transpose)
      val south = CommonUtils.transpose(west).map(x => Dish().tilt(x.reverse).reverse) //west -> south (1 transpose 1 reverse)
      val est = CommonUtils.transpose(south).map(x => Dish().tilt(x.reverse).reverse) // south -> est (1 transpose 1 reverse)
      est
    }
    sum_load(result)
  }

  def deja_vu(past: List[(Int, Int)], current: (Int, Int)): Boolean = {
    val is_deja_vu = past.filter((_, sum_past) => sum_past == current._2) match
      case list: List[(Int, Int)] if list.length > 1 =>
        val length_deja_vu = current._1 - list.head._1 / 2
        val string = past.map(x => x._2).slice(list.head._1, list.head._1 + length_deja_vu).mkString
        if string.take((length_deja_vu - 1) / 2) == string.takeRight((length_deja_vu - 1) / 2) then true
        else
          false
      case _ => false
    is_deja_vu
  }

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

  def call_state_end(input: List[String], loop: Int): Int = {
    loop match
      case 1 => call_sum(input)
      case _ => call_sum(input) + call_state_end(input, loop - 1)
  }
}
