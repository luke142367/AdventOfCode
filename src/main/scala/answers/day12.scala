package answers

import utils.FileHandler.readFile
import scala.collection.mutable.{Set => MSet}

object day12 {

  private val sample = """AAAA
                         |BBCD
                         |BBCC
                         |EEEC""".stripMargin

  private val sample2 = """RRRRIICCFF
                          |RRRRIICCCF
                          |VVRRRCCFFF
                          |VVRCCCJFFF
                          |VVVVCJJCFE
                          |VVIVCCJJEE
                          |VVIIICJJEE
                          |MIIIIIJJEE
                          |MIIISIJEEE
                          |MMMISSJEEE""".stripMargin

  private val sample3 = """EEEEE
                          |EXXXX
                          |EEEEE
                          |EXXXX
                          |EEEEE""".stripMargin

  private val sample4 = """AAAAAA
                          |AAABBA
                          |AAABBA
                          |ABBAAA
                          |ABBAAA
                          |AAAAAA""".stripMargin

  private def parseInput(input: String): Seq[Seq[Char]] = input.lines.map(_.toList.toSeq).toSeq

  private def searchArea(x: Int, y: Int, c: Char, grid: Seq[Seq[Char]], visited: MSet[(Int, Int)] = MSet()): Set[(Int, Int)] = {
    if (x < 0 || y < 0 || x >= grid.head.size || y >= grid.size || grid(y)(x) != c) return Set()
    if (visited.contains((x, y))) return Set((x, y))
    visited.add((x, y))

    val adj = Set((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).diff(visited)
    adj.flatMap((x, y) => searchArea(x, y, c, grid, visited)) ++ Set((x, y))
  }

  private def findAreas(grid: Seq[Seq[Char]]): Set[(Char, Set[(Int, Int)])] = {
    val pts = grid.indices.flatMap(y => grid.head.indices.map(x => (x, y)))

    pts.foldLeft((Set[(Char, Set[(Int, Int)])](), Set[(Int, Int)]())) {
      case ((areas, visited), pt) if visited.contains(pt) => (areas, visited)
      case ((areas, visited), (x, y)) =>
        val area = searchArea(x, y, grid(y)(x), grid)
        (Set((grid(y)(x), area)) ++ areas, area ++ visited)
    }._1
  }

  private def perimeter(area: Set[(Int, Int)]): Int =
    area.toSeq.map((x, y) => Set((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).diff(area).size).sum

  private def sides(area: Set[(Int, Int)]): Int = {
    val tops = area.filter((x, y) => !area.contains((x, y - 1))).groupBy(_._2).values.map(x => countContig(x.toSeq.map(_._1))).sum
    val bottoms = area.filter((x, y) => !area.contains((x, y + 1))).groupBy(_._2).values.map(x => countContig(x.toSeq.map(_._1))).sum
    val rights = area.filter((x, y) => !area.contains((x + 1, y ))).groupBy(_._1).values.map(x => countContig(x.toSeq.map(_._2))).sum
    val lefts = area.filter((x, y) => !area.contains((x - 1, y ))).groupBy(_._1).values.map(x => countContig(x.toSeq.map(_._2))).sum

    tops + bottoms + rights + lefts
  }

  private def countContig(nums: Seq[Int]): Int = nums match
    case Seq(_) => 1
    case _ => nums.sorted.sliding(2).count{case Seq(a, b) => (a - b).abs != 1} + 1

  private def partOne(grid: Seq[Seq[Char]]): Int = {
    val areas = findAreas(grid)
    areas.toSeq.map((_, pts) =>
      pts.size * perimeter(pts)
    ).sum
  }

  private def partTwo(grid: Seq[Seq[Char]]): Int = {
    val areas = findAreas(grid)
    areas.toSeq.map((_, pts) =>
      pts.size * sides(pts)
    ).sum
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("day12.txt")

    val grid = parseInput(input)

    val now = System.currentTimeMillis()
    val result = partTwo(grid)
    val taken = System.currentTimeMillis() - now

    println(result)
    println(s"Runtime: $taken ms")
  }
}
