package answers

import utils.FileHandler.readFile
import scala.collection.mutable.{Map => MMap}

object day11 {
  private val sample = "125 17"

  private def parseInput(input: String) = input.split(" ").map(_.toLong).toSeq

  private def splitStone(stone: Long): Seq[Long] = stone match
    case 0 => Seq(1)
    case x if x.toString.length % 2 == 0 => x.toString.splitAt(x.toString.length / 2) match
      case (a, b) => Seq(a.toLong, b.toLong)
    case x => Seq(x * 2024)

  private def splitStones(stones: Seq[Long], cache: MMap[(Long, Int), Long], blinks: Int): Long = {
    if (blinks == 0) return stones.size

    stones.map(stone =>
      val total = cache.getOrElse((stone, blinks), splitStones(splitStone(stone), cache, blinks - 1))
      cache += (((stone, blinks), total))
      total
    ).sum
  }

  private def partOne(stones: Seq[Long]): Long = splitStones(stones, MMap(), 25)

  private def partTwo(stones: Seq[Long]): Long = splitStones(stones, MMap(), 75)

  def main(args: Array[String]): Unit = {
    val input = readFile("day11.txt")
    val stones = parseInput(input)

    println(s"Part One: ${partOne(stones)}")
    println(s"Part Two: ${partTwo(stones)}")
  }
}
