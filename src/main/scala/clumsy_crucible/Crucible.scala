package clumsy_crucible

class Crucible {

  def find_path(source: List[List[Int]], min_move: Int, max_move: Int): Int = {
    val max_x = source.head.length - 1
    val max_y = source.length - 1
    val start = (0, 0)
    val end = (max_x, max_y)
    var visited: Map[(Int, Int), Int] = Map(start -> source.head.head)
    var next_queue: Seq[(Int, Int)] = Seq((0,0))

    def each_move(curr: (Int, Int)) = {
      val curr_cost = visited.getOrElse(curr, 0)
      val queue = for {
        (pos_x, pos_y) <- Seq((0, 1), (0, -1), (1, 0), (-1, 0))
        x = curr._1 + pos_x
        y = curr._2 + pos_y
        if x >= 0 && y >= 0 && x <= max_x && y <= max_y
      } yield {
        ((x, y), source(y)(x))
      }
      for {
        (point, new_cost) <- queue
      } yield {
        visited.get(point) match
          case Some(cost) =>
            if curr_cost + new_cost < cost then
              visited = visited.updated(point, curr_cost + new_cost)
              next_queue = next_queue :+ point
          case _ =>
            visited = visited.updated(point, curr_cost + new_cost)
            next_queue = next_queue :+ point
      }
      next_queue
    }

    while (next_queue.nonEmpty){
      val current_point = next_queue.head
      next_queue = next_queue.tail
      each_move(current_point)
    }

    source.length
  }
}
