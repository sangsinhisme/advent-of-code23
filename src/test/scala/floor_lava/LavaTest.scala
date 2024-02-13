package floor_lava
import utils.CommonUtils

import scala.annotation.tailrec
import scala.util.matching.Regex

class LavaTest extends munit.FunSuite:
  import floor_lava.Lava
  test("test is working?"){
    assert(1==1)
  }

  test("first heat mirror lava ?"){
    val source = CommonUtils.convert2list("src/test/scala/floor_lava/01.txt").map(x => x.toList)
    val result = Lava().heat_mirror(source, (0, 0), (1, 0))
    assert(result == 46)

  }

  test("part2 heat mirror lava ?"){
    val source = CommonUtils.convert2list("src/test/scala/floor_lava/01.txt").map(x => x.toList)
    val result = Lava().heat_mirror(source, (3, 0), (0, 1))
    assert(result == 51)

  }

  test ("equal vector ?"){
    val input = (1, 0)
    assert(input.equals((1, 0)))
  }

  test("replace .?") {
    def updateMap(map: List[List[Char]], visitedList: List[(Int, Int)]): List[List[Char]] = {
      visitedList.foldLeft(map)((m, coords) => {
        val (x, y) = coords
        if (x >= 0 && x < m.length && y >= 0 && y < m(x).length) {
          m.updated(x, m(x).updated(y, '.'))
        } else {
          m
        }
      })
    }

    // Example usage:
    val map = List(
      List('a', 'b', 'c'),
      List('d', 'e', 'f'),
      List('g', 'h', 'i')
    )
    val visitedList = List((0, 1), (1, 2), (2, 0))
    val updatedMap = updateMap(map, visitedList)
  }

  test("foreach all lava edge ?"){
    val source = CommonUtils.convert2list("src/test/scala/floor_lava/01.txt").map(x => x.toList)
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
      if (j == source.length - 1){
        val result = Lava().heat_mirror(source, (i, j), (0, -1))
        if result > max_result then max_result = result
      }
    }
    assert(max_result == 51)
  }

  test("final part 2 ?"){
    val source = CommonUtils.convert2list("src/test/scala/floor_lava/01.txt").map(x => x.toList)
    val result = Lava().find_max_reflected(source)
    assert(result == 51)
  }