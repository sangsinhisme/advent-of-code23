package lens_library

import scala.annotation.tailrec

class Hash {

  class Lens(var label: String, var length: Int) {
    override def toString: String = s"[$label $length]"
  }

  class Box(var index: Int, var lens: List[Lens]) {
    override def toString: String = {
      val lensString = lens.map(_.toString).mkString(" ")
      s"Box $index: [$lensString]"
    }
  }


  def hash_to_ascii(input: String): Int = {
    @tailrec
    def hash(input: String, curr: Int): Int = {
      if input.isEmpty then curr
      else hash(input.tail, (input.head.toByte + curr) * 17 % 256)
    }
    hash(input, 0)
  }

  def lens_hashmap(hash: String, boxes: List[Box]): List[Box] = {
    if (hash.contains("=")) {
      val label = hash.slice(0, hash.length - 2)
      val box_num = hash_to_ascii(label)
      val length = hash.takeRight(1).toInt
      val updatedBoxes = boxes.indexWhere(box => box.index == box_num) match {
        case index if index > -1 =>
          val lensIndex = boxes(index).lens.indexWhere(x => x.label == label)
          if (lensIndex != -1) {
            val new_box = boxes(index).lens.updated(lensIndex, Lens(label, length))
            boxes.updated(index, Box(box_num, new_box))
          } else {
            boxes.updated(index, Box(box_num, boxes(index).lens :+ Lens(label, length)))
          }
        case _ => boxes :+ Box(box_num, List(Lens(label, length)))
      }
      updatedBoxes
    } else {
      val label = hash.slice(0, hash.length - 1)
      val box_num = hash_to_ascii(label)
      val updatedBoxes = boxes.indexWhere(box => box.index == box_num) match {
        case index if index > -1 =>
          val remove_label = boxes(index).lens.filter(x => x.label != label)
          boxes.updated(index, Box(box_num, remove_label))
        case _ => boxes
      }
      updatedBoxes
    }
  }
}
