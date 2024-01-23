package point_incidence

import jdk.internal.util.xml.impl.Input

import scala.annotation.tailrec

class Mirror {

  def is_reflection(
      input_half: Array[String],
      output_half: Array[String]
  ): Boolean = {
    if (
      input_half.length != output_half.length
    ) {
      false
    } else {
      input_half.indices.forall { i =>
        input_half(i) == output_half(output_half.length - 1 - i)
      }
    }
  }

  def mirror_horizon(input: Array[String]): Int = {
    val length = input.length
    val mid = length / 2
    var mirror = 0
    for (i <- 1 until input.length) {
      if (i <= mid) {
        if is_reflection(input.take(i), input.slice(i, i + i)) then {
          mirror = i
        }
      } else {
        if is_reflection(input.slice(i - input.drop(i).length, i), input.drop(i)) then {
          mirror = i
        }
      }
    }
    mirror
  }

  def sumOfMirrorIndex(input: Array[String]): Int = {
    val horizon = mirror_horizon(input)
    val result = if horizon != 0 then horizon * 100
    else mirror_horizon(input.map(_.toList).toList.transpose.map(_.mkString).toArray)
    result
  }

  def calling_sum(
      source: Array[String],
      input_function: Array[String]
  ): Int = {
    if (source.isEmpty) {
      sumOfMirrorIndex(input_function)
    } else if (source.head.isEmpty) {
      calling_sum(source.tail, Array.empty) + sumOfMirrorIndex(input_function)
    } else {
      calling_sum(source.tail, input_function :+ source.head)
    }
  }
}
