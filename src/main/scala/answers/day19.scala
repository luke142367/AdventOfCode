package answers

import utils.FileHandler.readFile

import scala.collection.mutable.Map as MMap

object day19 {
  private val sample = """r, wr, b, g, bwu, rb, gb, br
                         |
                         |brwrr
                         |bggr
                         |gbbr
                         |rrbgbr
                         |ubwu
                         |bwurrg
                         |brgr
                         |bbrgwb""".stripMargin

  private def parseInput(input: String): (Set[String], Seq[String]) = {
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

  private def partOne(towels: Set[String], patterns: Seq[String]): Int =
    patterns.count(pattern => towelArrangements(pattern, towels) != 0)

  private def partTwo(towels: Set[String], patterns: Seq[String]): Long =
    patterns.map(pattern => towelArrangements(pattern, towels)).sum

  def main(args: Array[String]): Unit = {
    val input = readFile("day19.txt")
    val parsed = parseInput(input)

    val resultOne = partOne.tupled(parsed)
    val resultTwo = partTwo.tupled(parsed)

    println(s"Result One: $resultOne")
    println(s"Result Two: $resultTwo")
  }
}

extension (s: String)
  def hasPrefix(prefix: String): Boolean = s.take(prefix.length) == prefix
