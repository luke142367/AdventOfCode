package solutions.year2024

import utils.Day
import utils.Year.Year24

object day02 extends Day[Seq[Seq[Int]], Int, Int](Year24, 2) {
  
  private def isSafe(levels: Seq[Int]): Boolean = {
    val sortedLevels = levels.sorted
    if (sortedLevels != levels && sortedLevels != levels.reverse) {
      return false
    }

    !levels.zip(levels.tail).map { case (a, b) => (a - b).abs }.exists(diff => diff < 1 || diff > 3)
  }

  private def isSafer(levels: Seq[Int]): Boolean = isSafe(levels) || variations(levels).exists(isSafe)

  override def partOne(input: Seq[Seq[Int]]): Int = input.count(isSafe)

  override def partTwo(input: Seq[Seq[Int]]): Int = input.count(isSafer)

  private def variations(levels: Seq[Int]): Seq[Seq[Int]] =
    levels.indices.map(index => levels.zipWithIndex.filter(p => p._2 != index).map(_._1))


  def parseInput(input: String): Seq[Seq[Int]] = {
    val lines = input.split("\n").toSeq
    lines.map { line =>
      line.split(" ").map(_.toInt)
    }
  }
}