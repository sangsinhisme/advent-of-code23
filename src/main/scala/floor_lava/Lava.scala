package floor_lava

class Lava {

  def update_map(map: List[List[Char]], visitedList: List[(Int, Int)]): List[List[Char]] = {
    visitedList.foldLeft(map)((m, coords) => {
      val (y, x) = coords
      if (x >= 0 && x < m.length && y >= 0 && y < m(x).length) {
        m.updated(x, m(x).updated(y, '#'))
      } else {
        m
      }
    })
  }

  def heat_mirror(map: List[List[Char]], start_point: (Int, Int), vector_init: (Int, Int)): Int = {
    val x_max = map.head.length
    val y_max = map.length
    var visited: List[(Int, Int)] = List.empty
    var memo: Map[(Int, Int), List[(Int, Int)]] = Map.empty
    def reflect(pointy: (Int, Int), vector: (Int, Int)): Unit = {
      val (x, y) = (pointy._1, pointy._2)
      if (x < x_max && y < y_max  && x > -1 && y > - 1) {
        val (x_next, y_next) = (pointy._1 + vector._1, pointy._2 + vector._2)
        if (visited.indexWhere(point => point._1 == x && point._2 == y) == -1)
          visited = visited :+ pointy

        val is_visited = memo.get(pointy) match {
          case Some(values) => values.contains(vector)
          case None => false
        }
        if (!is_visited) {
          memo = memo.updated(pointy, memo.getOrElse(pointy, List.empty) :+ vector)
          map(y)(x) match
            case '.' => reflect((x_next, y_next), vector)
            case '|' =>
              if vector._1 == 0 then
                reflect((x_next, y_next), vector)
              else
                reflect((x, y + 1), (0, 1))
                reflect((x, y - 1), (0, -1))
            case '-' =>
              if vector._2 == 0 then
                reflect((x_next, y_next), vector)
              else
                reflect((x + 1, y), (1, 0))
                reflect((x - 1, y), (-1, 0))
            case '\\' =>
              val (vector_x, vector_y) = (vector._2, vector._1)
              reflect((x + vector_x, y + vector_y), (vector_x, vector_y))
            case _ =>
              val (vector_x, vector_y) = (-vector._2, -vector._1)
              reflect((x + vector_x, y + vector_y), (vector_x, vector_y))
        }
      }
    }
    reflect(start_point, vector_init)

    visited.length
  }
  
  def find_max_reflected(source: List[List[Char]]): Int = {
    var max_result = 0
    for {
      i <- source.head.indices
      j <- source.indices
    } yield {
      if (i == 0) {
        val result = Lava().heat_mirror(source, (i, j), (1, 0))
        if result > max_result then max_result = result
      }
      if (j == 0) {
        val result = Lava().heat_mirror(source, (i, j), (0, 1))
        if result > max_result then max_result = result
      }
      if (i == source.head.length - 1) {
        val result = Lava().heat_mirror(source, (i, j), (-1, 0))
        if result > max_result then max_result = result
      }
      if (j == source.length - 1) {
        val result = Lava().heat_mirror(source, (i, j), (0, -1))
        if result > max_result then max_result = result
      }
    }
    max_result
  }
}
