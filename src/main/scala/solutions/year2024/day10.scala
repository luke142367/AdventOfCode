package solutions.year2024

import utils.{Day, Year}
import utils.Year.Year24

import scala.annotation.tailrec

case class Topo(x: Int, y: Int, height: Int)

object day10 extends Day[Seq[Seq[Int]], Int, Int](Year24, 10) {
  private val sample = """89010123
                         |78121874
                         |87430965
                         |96549874
                         |45678903
                         |32019012
                         |01329801
                         |10456732""".stripMargin

  def parseInput(input: String): Seq[Seq[Int]] = input.linesIterator.map(_.toList.map(_.toString.toInt)).toSeq

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

  def partOne(grid: Seq[Seq[Int]]): Int = {
    val topos = computeTopos(grid)

    computeTrailheads(topos, 9, topos.filter(_.height == 9).map(topo => (topo, 1)))
  }
}
