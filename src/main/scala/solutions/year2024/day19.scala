package solutions.year2024

import utils.Day
import utils.Year.Year24

import scala.collection.mutable.Map as MMap

object day19 extends Day[(Set[String], Seq[String]), Int, Long](Year24, 19) {
  def parseInput(input: String): (Set[String], Seq[String]) = {
    val Seq(towels, patterns) = input.split("\n\n").toSeq
    (towels.split(", ").toSet, patterns.linesIterator.toSeq)
  }

  private def towelArrangements(pattern: String, towels: Set[String], results: MMap[String, Long] = MMap()): Long = {
    if (pattern.isEmpty) return 1
    if (results.contains(pattern)) return results(pattern)

    towels
      .filter(towel => pattern.hasPrefix(towel))
      .foldLeft(0L)((total, prefix) =>
        val result = towelArrangements(pattern.stripPrefix(prefix), towels, results)
        results += ((pattern.stripPrefix(prefix), result))
        total + result
      )
  }

  override def partOne(input: (Set[String], Seq[String])): Int = input match
    case (towels: Set[String], patterns: Seq[String]) =>
      patterns.count(pattern => towelArrangements(pattern, towels) != 0)

  override def partTwo(input: (Set[String], Seq[String])): Long = input match
    case (towels: Set[String], patterns: Seq[String]) =>
      patterns.map(pattern => towelArrangements(pattern, towels)).sum
}

extension (s: String)
  def hasPrefix(prefix: String): Boolean = s.take(prefix.length) == prefix
