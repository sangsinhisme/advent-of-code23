package pipe_maze

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object part2 {

  def update_vector(current_move: Char, vector: (Int, Int)):  (Int, Int) = {
    current_move match
      case '|' => vector
      case '-' => vector
      case 'L' => (vector._2, vector._1)
      case 'J' => (vector._2 * -1, vector._1 * -1)
      case '7' => (vector._2, vector._1)
      case 'F' => (vector._2 * -1, vector._1 * -1)
      case 'S' => (0, 0)
  }

  def find_in_pipe(current_move: (Int, Int), graph_pipe_index: Array[(Int, Int)], graph: Array[Array[Char]], max_length: Int): Boolean = {
    var stack: Char = '.'
    var stack_open = 0
    for (i <-  current_move._1 + 1 until max_length) {
      graph_pipe_index.indexWhere(x => x._1 == i && x._2 == current_move._2) match
        case -1 =>
        case _ =>
          graph(current_move._2)(i) match
          case '|' => if stack_open == 0 then stack_open += 1 else stack_open -= 1
          case 'L' => stack = 'L'
          case 'J' => if stack == 'L' then {
            stack = '.'
          } else if stack == 'F' then {
            if stack_open == 0 then stack_open += 1 else stack_open -= 1
            stack = '.'
          }
          case '7' => if stack == 'F' then {
            stack = '.'
          } else if stack == 'L' then {
            if stack_open == 0 then stack_open += 1 else stack_open -= 1
            stack = '.'
          }
          case 'F' => stack = 'F'
          case _ =>
    }
    stack_open % 2 == 1
  }

  def update_next_move(current_move: (Int, Int), vector: (Int, Int), graph: Array[Array[Char]]): (Char, (Int, Int), (Int, Int)) = {
    val next_move_index = (current_move._1 + vector._1, current_move._2 + vector._2)
    val character = graph(next_move_index._2)(next_move_index._1)
    val next_move_vector = update_vector(character, vector)
    (character, next_move_index, next_move_vector)
  }

  def fill_flood(un_visited: Array[(Int, Int)], graph_pipe_index: Array[(Int, Int)], graph: Array[Array[Char]]): Array[(Int, Int)] = {
    var queue: Array[(Int, Int)] = un_visited
    var visited: Array[(Int, Int)] = Array.empty
    val offsets = Seq((-1, 0), (1, 0), (0, -1), (0, 1))
    while (queue.nonEmpty) {
      val current_move = queue.last
      visited = visited :+ current_move
      queue = queue.dropRight(1)
      for ((i, j) <- offsets) {
        val x = current_move._1 + i
        val y = current_move._2 + j
        if (x > -1 && y > -1 && x < graph(0).length && y < graph.length) {
          graph.apply(y).apply(x) match
            case pipe if pipe.equals('.') =>
              visited.indexWhere(point => point._1 == x && point._2 == y) match
                case is_visited if is_visited == -1 =>
                  if find_in_pipe((x, y), graph_pipe_index, graph, graph(0).length) then queue = queue :+ (current_move._1 + i, current_move._2 + j)
                case _ =>
            case _ =>
        }
        queue = queue.distinct
      }
    }
    visited.distinct
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("src/main/scala/pipe_maze/puzzle_day10.txt")
    val lines =
      try source.getLines().toList
      finally source.close()

    var graph: Array[Array[Char]] = Array.empty
    var graph_pipe_index: Array[(Int, Int)] = Array.empty

    var start = (0, 0)
    for (line <- lines) {
      graph = graph :+ line.toArray
      start = line.indexWhere(x => x == 'S') match
        case x if x > -1 => (x, graph.length - 1)
        case _ => start
    }
    var result = update_next_move(start, vector = (0, 1), graph)
    graph_pipe_index = graph_pipe_index :+ start
    while (result._3 != (0, 0)) {
      graph_pipe_index = graph_pipe_index :+ result._2
      result = update_next_move(result._2, result._3, graph)
    }

    for (i <- graph(0).indices) {
      for (j <- graph.indices) {
        graph_pipe_index.indexWhere(x => x._1 == i && x._2 == j) match {
          case -1 => graph(j)(i) = '.'
          case _ =>
        }
      }
    }

    var un_visited: Array[(Int, Int)] = Array.empty
    for (i <- graph_pipe_index.indices) {
      val (x, y) = graph_pipe_index.apply(i)
      if (graph(y)(x) == 'L' || graph(y)(x) == 'J' || graph(y)(x) == '7' || graph(y)(x) == 'F') {
        for (i <- Seq(-1, 1)) {
          for (j <- Seq(-1, 1)) {
            if (x + i > -1 && y + j > -1 && x + i < graph(0).length && y + j < graph.length) {
              graph(y + j)(x + i) match
                case '.' if find_in_pipe((x + i, y + j), graph_pipe_index, graph, graph(0).length) => un_visited = un_visited :+ (x + i, y + j)
                case _ =>
            }
          }
        }
      }
    }
    val _result = fill_flood(un_visited.distinct, graph_pipe_index, graph)
    println(_result.length)
  }
}
