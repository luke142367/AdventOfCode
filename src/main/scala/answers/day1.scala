package answers

import utils.FileHandler.readFile

object day1 {
  private def partOne(first: Seq[Int], second: Seq[Int]): Int = first.sorted.zip(second.sorted).map { case (a, b) => (a-b).abs}.sum

  private def partTwo(first: Seq[Int], second: Seq[Int]): Int = {
    val occurrences = second.groupBy(identity)
    first.map(num => occurrences.getOrElse(num, Seq()).size * num).sum
  }

  private def parseInput(input: String): (Seq[Int], Seq[Int]) = {
    val lines = input.split("\n").toSeq
    lines.map { line =>
      val numbers = line.split(" ")
      (numbers.head.toInt, numbers.last.toInt)
    }.unzip
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("day1.txt")
    val parsed = parseInput(input)
    val result = partTwo(parsed._1, parsed._2)
    println(result)
  }
}

