package pipe_maze

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Main {

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

  def update_next_move(current_move: (Int, Int), vector: (Int, Int), graph: Array[Array[Char]]): (Char, (Int, Int), (Int, Int)) = {
    val next_move_index = (current_move._1 + vector._1, current_move._2 + vector._2)
    val character = graph.apply(next_move_index._2).apply(next_move_index._1)
    val next_move_vector = update_vector(character, vector)
    (character, next_move_index, next_move_vector)
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("src/main/scala/pipe_maze/puzzle_day10.txt")
    val lines =
      try source.getLines().toList
      finally source.close()

    var graph: Array[Array[Char]] = Array.empty
    var start = (0, 0)
    for (line <- lines) {
      graph = graph :+ line.toArray
      start = line.indexWhere(x => x == 'S') match
        case x if x > -1 => (x, graph.length - 1)
        case _ => start
    }
    var result = update_next_move(start, vector = (0, 1), graph)
    var distinct = 1
    while (result._3 != (0, 0)) {
      result = update_next_move(result._2, result._3, graph)
      distinct += 1
    }
    println(distinct/2)
  }
}
