package solutions.year2024

import utils.Day
import utils.Year.Year24

import scala.annotation.tailrec

object day22 extends Day[Seq[Long], Long, Long](Year24, 22) {
  private val prune = 16777216L

  def parseInput(input: String): Seq[Long] = input.linesIterator.map(_.toLong).toSeq

  private def nextSecret(current: Long): Long = {
    val mul = ((current * 64) ^ current) % prune
    val div = ((mul / 32) ^ mul) % prune
    ((div * 2048) ^ div) % prune
  }

  @tailrec
  private def repeatSecret(start: Long, times: Int): Long = {
    if (times == 0) return start

    repeatSecret(nextSecret(start), times - 1)
  }

  private def allSecrets(start: Long, times: Int): Seq[Long] =
    (1 to times).foldLeft((start, Seq[Long]())) { case ((prev, soFar), _) =>
      val next = nextSecret(prev)
      (next, next +: soFar)
    }._2.reverse

  private def price(together: Map[Seq[Long], Long], changes: Seq[Long]): Option[Long] = together.get(changes)

  override def partOne(starts: Seq[Long]): Long = starts.map(start => repeatSecret(start, 2000)).sum

  override def partTwo(starts: Seq[Long]): Long = {
    val secrets = starts
      .map(start => start +: allSecrets(start, 2000))
      .map(secrets => secrets.map(_ % 10))

    val diffs = secrets.map(secret => secret.sliding(2).map {case Seq(from, to) => to - from}.sliding(4).toSeq)

    val changes = diffs.flatten.toSet

    val together = secrets.indices.map(i => diffs(i).zip(secrets(i).drop(4))).map(tog => tog.distinctBy(_._1))

    val maps = together.map(tog => tog.toMap)

    changes.toSeq.map(change => maps.flatMap(tog => price(tog, change)).sum).max
  }
}
