package solutions.year2024

import utils.{Day, Year}
import utils.Year.Year24

object day01 extends Day[(Seq[Int], Seq[Int]), Int, Int](Year24, 1) {
  override def parseInput(input: String): (Seq[Int], Seq[Int]) = {
    val lines = input.split("\n").toSeq
    lines.map { line =>
      val numbers = line.split(" ")
      (numbers.head.toInt, numbers.last.toInt)
    }.unzip
  }

  override def partOne(input: (Seq[Int], Seq[Int])): Int = input match
    case (first, second) => first.sorted.zip(second.sorted).map { case (a, b) => (a-b).abs}.sum

  override def partTwo(input: (Seq[Int], Seq[Int])): Int = {
    val (first, second) = input
    val occurrences = second.groupBy(identity)
    first.map(num => occurrences.getOrElse(num, Seq()).size * num).sum
  }
}

