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
    val result = Lava().heat_mirror(source)
    assert(result == 46)

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