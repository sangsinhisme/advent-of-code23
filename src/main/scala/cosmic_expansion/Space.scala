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

  def space_expands_multiple(space: Array[Array[Char]]): Array[Array[Char]] = {
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
          temp = temp :+ 'x'
        else
          temp = temp :+ space(y)(x)
      }
      if (galaxies_y contains y)
        galaxies_result = galaxies_result :+ temp.map(_ => 'x')
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


  def steps(source: (Int, Int), dest: (Int, Int), space: Array[Array[Char]], multiple: Int = 1): Long = {
    val (_x, _y) = (
      if (source._1 < dest._1) 1 else -1,
      if (source._2 < dest._2) 1 else -1
    )

    if (source._1 == dest._1 ) {
      val expand_y = (for {
        i <- source._2 until dest._2
      } yield {
        if (space(i)(source._1) == 'x') multiple - 1 else 0
      }).sum
      steps(source, dest) + expand_y
    }
    else if (source._2 == dest._2) {
      val expand_x = (for {
        i <- source._1 until dest._1
      } yield {
        if (space(source._2)(i) == 'x') multiple - 1 else 0
      }).sum
      steps(source, dest) + expand_x
    }
    else{
      val xRange = source._1 + _x to dest._1 by _x
      val yRange = source._2 + _y to dest._2 by _y
      val result =
      (for {
        i <- xRange.indices
      } yield {
        if (space(yRange.last)(xRange(i)) == 'x') multiple else 1
      }).sum
        +
        (for {
          y <- yRange.indices
        } yield {
          if (space(yRange(y))(xRange.head - _x) == 'x') multiple else 1
        }).sum
      result
    }

  }

}
