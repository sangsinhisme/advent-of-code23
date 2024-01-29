package lens_library

import scala.annotation.tailrec

class Hash {

  def hash_to_ascii(input: String): Int = {
    @tailrec
    def hash(input: String, curr: Int): Int = {
      if input.isEmpty then curr
      else hash(input.tail, (input.head.toByte + curr) * 17 % 256)
    }
    hash(input, 0)
  }
}
