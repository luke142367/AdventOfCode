package answers

import utils.FileHandler.readFile

import scala.annotation.tailrec

case class Topo(x: Int, y: Int, height: Int)

object dayTen {

  private val sample = """89010123
                         |78121874
                         |87430965
                         |96549874
                         |45678903
                         |32019012
                         |01329801
                         |10456732""".stripMargin

  private def parseInput(input: String): Seq[Seq[Int]] = input.linesIterator.map(_.toList.map(_.toString.toInt)).toSeq

  private def computeTopos(grid: Seq[Seq[Int]]): Set[Topo] = {
    grid.zipWithIndex.flatMap((line, y) =>
      line.zipWithIndex.map((height, x) => Topo(x, y, height))
    ).toSet
  }

  @tailrec
  private def computeTrailheads(topos: Set[Topo], height: Int, previousTopos: Set[(Topo, Int)]): Int = {
    if (height == 0) return previousTopos.toSeq.map(_._2).sum

    val adjacents = previousTopos.toSeq.flatMap((topo, tops) =>
      Seq(
        (Topo(topo.x - 1, topo.y, topo.height - 1), tops),
        (Topo(topo.x + 1, topo.y, topo.height - 1), tops),
        (Topo(topo.x, topo.y - 1, topo.height - 1), tops),
        (Topo(topo.x, topo.y + 1, topo.height - 1), tops),
      )
    )
      .filter((topo, _) => topos.contains(topo))
      .groupBy(_._1)
      .map((topo, tops) => (topo, tops.map(_._2).sum))
      .toSet

    computeTrailheads(topos, height - 1, adjacents)
  }

  private def partOne(grid: Seq[Seq[Int]]): Int = {
    val topos = computeTopos(grid)

    computeTrailheads(topos, 9, topos.filter(_.height == 9).map(topo => (topo, 1)))
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("day10.txt")

    val grid = parseInput(input)


    val now = System.currentTimeMillis()
    val result = partOne(grid)
    val taken = System.currentTimeMillis() - now

    println(result)
    println(s"Runtime: $taken ms")
  }
}
