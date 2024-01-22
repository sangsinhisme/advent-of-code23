package point_incidence

import scala.annotation.tailrec

class Mirror {
  def sumOfMirrorIndex(input: Array[String]): Int = {
    @tailrec
    def have_reflection(head: String, tail: Array[String]): Boolean = {
      if tail.isEmpty then false
      else if head.equals(tail.head) then true
      else have_reflection(head, tail.tail)
    }
    @tailrec
    def find_mirror(head: String, tail: Array[String], idx: Int): Int = {
      if tail.isEmpty then idx
      else if have_reflection(head, tail) then find_mirror(tail.head, tail.tail, idx + 1)
      else if idx > 0 then idx
      else find_mirror(tail.head, tail, idx)
    }
    val result = find_mirror(input.head, input.tail, 0)
    if result != 0 then result * 100
    else {
      val transpose = input.map(_.toList).toList.transpose.map(_.mkString).toArray
      find_mirror(transpose.head, transpose.tail, 0)
    }
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
