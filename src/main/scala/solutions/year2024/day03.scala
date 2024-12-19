package solutions.year2024

import utils.{Day, Year}
import utils.FileHandler.readFile
import utils.Year.Year24

import scala.util.matching.Regex

object day03 extends Day[String, Int, Int](Year24, 3) {
  private val regexOne: Regex = "mul\\((\\d{1,3}),(\\d{1,3})\\)".r
  private val regexTwo: Regex = "mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don't\\(\\)".r

  private val sampleTwo: String = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

  def partOne(input: String): Int = regexOne.findAllMatchIn(input).map(m => m.group(1).toInt * m.group(2).toInt).sum

  private def partTwo1(input: String): Int = {
    val matches = regexTwo.findAllMatchIn(input)
    var valid = true
    matches.map { m =>
      m.group(0) match
        case "do()" =>
          valid = true
          0
        case "don't()" =>
          valid = false
          0
        case _ => if valid then m.group(1).toInt * m.group(2).toInt else 0
    }.sum
  }

  override def partTwo(input: String): Int = {
    val matches = regexTwo.findAllMatchIn(input)
    val donts = split(matches.toSeq, m => m.group(0) == "don't()")

    val dos = donts.tail.map(dont => split(dont, m => m.group(0) == "do()")).flatMap(_.tail).flatten

    (donts.head ++ dos).map(m => m.group(1).toInt * m.group(2).toInt).sum
  }

  private def partTwo3(input: String): Int = {
    val matches = regexTwo.findAllMatchIn(input)
    val x = matches.map(_.group(0)).mkString
    val donts = x.split("don't()").toSeq
    val dos = donts.tail.map(dont => dont.split("do()").toSeq.tail).mkString
    partOne(donts.head + dos)
  }

  private def split(matches: Seq[Regex.Match], matcher: (Regex.Match => Boolean)): Seq[Seq[Regex.Match]] =
    matches.foldRight(Seq[Seq[Regex.Match]]()) {
      case (m, head :: tail) => if matcher(m) then Seq() +: head +: tail else (m +: head) +: tail
      case (m, Seq()) => if matcher(m) then Seq() else Seq(Seq(m))
    }

  override def parseInput(input: String): String = input
}
