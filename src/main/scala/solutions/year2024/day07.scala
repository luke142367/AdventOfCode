package solutions.year2024

import utils.Year.Year24
import utils.{Day, Year}

object day07 extends Day[Seq[(Long, Seq[Long])], Long, Long](Year24, 7) {
  private val sample = """190: 10 19
                 |3267: 81 40 27
                 |83: 17 5
                 |156: 15 6
                 |7290: 6 8 6 15
                 |161011: 16 10 13
                 |192: 17 8 14
                 |21037: 9 7 18 13
                 |292: 11 6 16 20""".stripMargin

  def parseInput(input: String): Seq[(Long, Seq[Long])] = input.linesIterator.map(line =>
      val sections = line.split(": ")
      (sections.head.toLong, sections.last.split(" ").map(_.toLong).toSeq)
    ).toSeq

  private def validEquation(target: Long, nums: Seq[Long], useConcat: Boolean): Boolean = {
    if (nums.size == 1) return nums.head == target

    val mul = target % nums.head == 0 && validEquation(target / nums.head, nums.tail, useConcat)
    val add = target > nums.head && validEquation(target - nums.head, nums.tail, useConcat)
    val concat =
      nums.size >= 2
        && target.toString.length > nums.head.toString.length
        && target.toString.endsWith(nums.head.toString)
        && validEquation(target.toString.dropRight(nums.head.toString.length).toLong, nums.tail, useConcat)

    add || mul || (useConcat && concat)
  }

  override def partOne(eqs: Seq[(Long, Seq[Long])]): Long =
    eqs.filter((target, nums) => validEquation(target, nums.reverse, false)).map(_._1).sum

  override def partTwo(eqs: Seq[(Long, Seq[Long])]): Long =
    eqs.filter((target, nums) => validEquation(target, nums.reverse, true)).map(_._1).sum
}
