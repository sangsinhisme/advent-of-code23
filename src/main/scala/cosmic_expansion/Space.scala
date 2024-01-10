package cosmic_expansion

class Space {
  def space_expands(space: Array[Array[Char]]): Array[Array[Char]] = {
    var galaxies_x: Array[Int] = Array.empty
    var galaxies_y: Array[Int] = Array.empty
    var galaxies_result: Array[Array[Char]] = Array.empty
    for (y <- space.indices) {
      space(y).indexWhere(char => char == '#') match
        case index if index > -1 =>
        case _ => galaxies_y = galaxies_y :+ y
    }
    for (x <- space(0).indices) {
      var temp: Array[Char] = Array.empty
      for (y <- space.indices) {
        temp = temp :+ space(y)(x)
      }
      temp.indexWhere(char => char == '#') match
        case index if index > -1 =>
        case _ => galaxies_x = galaxies_x :+ x
    }
    for (y <- space.indices) {
      var temp: Array[Char] = Array.empty
      for (x <- space(0).indices) {
        if (galaxies_x contains x)
          temp = temp :+ space(y)(x) :+ space(y)(x)
        else
          temp = temp :+ space(y)(x)
      }
      if (galaxies_y contains y)
        galaxies_result = galaxies_result :+ temp :+ temp
      else
        galaxies_result = galaxies_result :+ temp
    }
    galaxies_result
  }

  def find_galaxies(space: Array[Array[Char]]): Array[(Int, Int)] = {
    val coordinates = for {
      i <- space(0).indices
      j <- space.indices
      if space(j)(i) == '#'
    } yield (i, j)

    coordinates.toArray
  }

  def steps(source: (Int, Int), dest: (Int, Int)): Int = {
    Math.abs(source._1 - dest._1) + Math.abs(source._2 - dest._2)
  }

}
