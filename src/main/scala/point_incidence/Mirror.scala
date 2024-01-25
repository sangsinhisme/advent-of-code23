package point_incidence

import jdk.internal.util.xml.impl.Input

import scala.annotation.tailrec

class Mirror {

  def is_reflection(
      input_half: Array[String],
      output_half: Array[String]
  ): Boolean = {
    if (input_half.length != output_half.length) {
      false
    } else {
      input_half.indices.forall { i =>
        input_half(i) == output_half(output_half.length - 1 - i)
      }
      if (fix_smudge(input_half, output_half)) true
      else false
    }
  }

  def mirror_horizon(input: Array[String], i: Int = 1): Int = {
    val length = input.length
    val mid = length / 2
    if (i < input.length) {
      val isReflection =
        if (i <= mid) is_reflection(input.take(i), input.slice(i, i + i))
        else
          is_reflection(input.slice(i - input.drop(i).length, i), input.drop(i))
      if (isReflection) i
      else mirror_horizon(input, i + 1)
    } else {
      0
    }
  }

  def fix_smudge(
      input_half: Array[String],
      output_half: Array[String]
  ): Boolean = {
    val reflect = input_half.indices.map { i =>
      input_half(i).zip(output_half(output_half.length - 1 - i)).count {
        case (c1, c2) => c1 == c2
      }
    }.sum

    if (reflect == (input_half.length * input_half.head.length - 1)) {
      true
    } else false
  }

  def sumOfMirrorIndex(input: Array[String]): Int = {
    val horizon = mirror_horizon(input)
    val vertical = mirror_horizon(
      input.map(_.toList).toList.transpose.map(_.mkString).toArray
    )
    horizon * 100 + vertical
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
