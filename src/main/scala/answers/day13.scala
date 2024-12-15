package answers

import utils.FileHandler.readFile

import scala.annotation.targetName
import scala.util.matching.Regex
import math.min
import scala.collection.mutable.{Map => MMap}

case class xy(x: Long, y: Long) {
  @targetName("minus")
  def -(other: xy): xy = xy(x - other.x, y - other.y)
}
case class Machine(a: xy, b: xy, prize: xy)

object dayThirteen {
  private val sample = """Button A: X+94, Y+34
                         |Button B: X+22, Y+67
                         |Prize: X=8400, Y=5400
                         |
                         |Button A: X+26, Y+66
                         |Button B: X+67, Y+21
                         |Prize: X=12748, Y=12176
                         |
                         |Button A: X+17, Y+86
                         |Button B: X+84, Y+37
                         |Prize: X=7870, Y=6450
                         |
                         |Button A: X+69, Y+23
                         |Button B: X+27, Y+71
                         |Prize: X=18641, Y=10279""".stripMargin

  private val inputReg: Regex = "[XY][+=](\\d+)".r

  private val partTwoAdd = 10000000000000L

  private def parseInput(input: String): Seq[Machine] = {
    input.split("\n\n").map(section =>
      val Seq(a, b, prize) = section.linesIterator.map( line =>
        val Seq(x,y) = inputReg.findAllMatchIn(line).map(_.group(1).toLong).toSeq
        xy(x, y)
      ).toSeq
      Machine(a, b, prize)
    )
  }

  private def findMinimalTokens(machine: Machine): Option[Long] = {
    val det = machine.a.x * machine.b.y - machine.a.y * machine.b.x
    if (det == 0) return None

    val a = machine.prize.x * machine.b.y - machine.b.x * machine.prize.y
    val b = machine.a.x * machine.prize.y - machine.a.y * machine.prize.x

    if (a % det == 0 && b % det == 0) {
      Some(a / det * 3 + b / det)
    } else {
      None
    }
  }

  private def partOne(machines: Seq[Machine]): Long = machines.flatMap(findMinimalTokens).sum

  private def partTwo(machines: Seq[Machine]): Long = machines
      .map(m => Machine(m.a, m.b, xy(m.prize.x + partTwoAdd, m.prize.y + partTwoAdd)))
      .flatMap(findMinimalTokens)
      .sum

  def main(args: Array[String]): Unit = {
    val input = readFile("day13.txt")

    val machines = parseInput(input)
    
    val result = partTwo(machines)

    println(result)
  }
}
