package solutions.year2024

import utils.{Day, Year}
import utils.Year.Year24

import scala.collection.mutable.Map as MMap

object day11 extends Day[Seq[Long], Long, Long](Year24, 11) {
  private val sample = "125 17"

  def parseInput(input: String): Seq[Long] = input.split(" ").map(_.toLong).toSeq

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

  override def partOne(stones: Seq[Long]): Long = splitStones(stones, MMap(), 25)

  override def partTwo(stones: Seq[Long]): Long = splitStones(stones, MMap(), 75)
}
