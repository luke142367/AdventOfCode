package answers

import utils.FileHandler.readFile

object day2 {
  private def isSafe(levels: Seq[Int]): Boolean = {
    val sortedLevels = levels.sorted
    if (sortedLevels != levels && sortedLevels != levels.reverse) {
      return false
    }

    !levels.zip(levels.tail).map { case (a, b) => (a - b).abs }.exists(diff => diff < 1 || diff > 3)
  }

  private def isSafer(levels: Seq[Int]): Boolean = isSafe(levels) || variations(levels).exists(isSafe)

  private def partOne(input: Seq[Seq[Int]]): Int = input.count(isSafe)

  private def partTwo(input: Seq[Seq[Int]]): Int = input.count(isSafer)

  private def variations(levels: Seq[Int]): Seq[Seq[Int]] =
    levels.indices.map(index => levels.zipWithIndex.filter(p => p._2 != index).map(_._1))


  def parseInput(input: String): Seq[Seq[Int]] = {
    val lines = input.split("\n").toSeq
    lines.map { line =>
      line.split(" ").map(_.toInt)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("day2.txt")
    val parsed = parseInput(input)
    val numSafe = partTwo(parsed)
    println(numSafe)
  }
}