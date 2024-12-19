package solutions.year2017

import utils.Day
import utils.Year.Year17

import scala.annotation.tailrec

object day12 extends Day[Seq[(Int, Set[Int])], Int, Int](Year17, 12) {
  override def parseInput(input: String): Seq[(Int, Set[Int])] =
    input.linesIterator.map(line =>
      val Seq(from, to) = line.split(" <-> ").toSeq
      (from.toInt, to.split(", ").map(_.toInt).toSet)
    ).toSeq

  @tailrec
  private def findAllReachable(associations: Map[Int, Set[Int]], current: Set[Int]): Set[Int] = {
    val next = current.flatMap(start => associations(start))
    if (next == current) return current

    findAllReachable(associations, next)
  }

  override def partOne(input: Seq[(Int, Set[Int])]): Int = findAllReachable(input.toMap, Set(0)).size

  override def partTwo(input: Seq[(Int, Set[Int])]): Int = {
    input.map((start, _) => findAllReachable(input.toMap, Set(start))).toSet.size
  }
}
