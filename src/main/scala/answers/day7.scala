package answers

import utils.FileHandler.readFile

object day7 {
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

  private def validEquation(target: Long, nums: Seq[Long]): Boolean = {
    if (nums.size == 1) return nums.head == target

    val mul = target % nums.head == 0 && validEquation(target / nums.head, nums.tail)
    val add = target > nums.head && validEquation(target - nums.head, nums.tail)
    val concat =
      nums.size >= 2
        && target.toString.length > nums.head.toString.length
        && target.toString.endsWith(nums.head.toString)
        && validEquation(target.toString.dropRight(nums.head.toString.length).toLong, nums.tail)

    add || mul || concat
  }

  private def partOne(eqs: Seq[(Long, Seq[Long])]): Long =
    eqs.filter((target, nums) => validEquation(target, nums.reverse)).map(_._1).sum

  def main(args: Array[String]): Unit = {
    val input = readFile("day7.txt")

    val parsed = parseInput(input)

    val now = System.currentTimeMillis()
    val result = partOne(parsed)
    val taken = System.currentTimeMillis() - now

    println(result)
    println(s"Runtime: $taken ms")
  }
}
