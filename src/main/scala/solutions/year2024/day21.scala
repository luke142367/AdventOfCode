package solutions.year2024

import utils.Day
import utils.SequenceUtils.permutations
import utils.Year.Year24

import scala.annotation.tailrec
import scala.collection.mutable.Map as MMap

object day21 extends Day[Seq[String], Long, Long](Year24, 21) {
  private def numKeyPosition(char: Char): (Int, Int) = char match
    case '7' => (0, 0)
    case '8' => (1, 0)
    case '9' => (2, 0)
    case '4' => (0, 1)
    case '5' => (1, 1)
    case '6' => (2, 1)
    case '1' => (0, 2)
    case '2' => (1, 2)
    case '3' => (2, 2)
    case '0' => (1, 3)
    case 'A' => (2, 3)

  private def dirKeyPosition(char: Char): (Int, Int) = char match
    case '^' => (1, 0)
    case 'A' => (2, 0)
    case '<' => (0, 1)
    case 'v' => (1, 1)
    case '>' => (2, 1)

  def parseInput(input: String): Seq[String] = input.linesIterator.toSeq

  private def keySeq(key: Direction, times: Int): Seq[Char] = (1 to times).map(_ => key.toChar)

  private def betweenKeys(start: Char, end: Char, lookup: Char => (Int, Int), hole: (Int, Int)): Set[Seq[Char]] = {
    val (startX, startY) = lookup(start)
    val (endX, endY) = lookup(end)

    val right = startX < endX
    val down = startY < endY

    val horizontal = keySeq(if (right) Direction.Right else Direction.Left, (startX - endX).abs)
    val vertical = keySeq(if (down) Direction.Down else Direction.Up, (startY - endY).abs)

    permutations(horizontal, vertical).filter(moves => validMoves(moves, lookup(start), hole)).map(_ ++ Seq('A'))
  }

  private def numBetweenKeys(start: Char, end: Char): Set[Seq[Char]] = betweenKeys(start, end, numKeyPosition, (0, 3))

  private def dirBetweenKeys(start: Char, end: Char): Set[Seq[Char]] = betweenKeys(start, end, dirKeyPosition, (0, 0))

  private def computeFirstInputs(key: String, times: Int): Seq[Set[Seq[Char]]] =
    ('A' +: key.toList).sliding(2).map { case Seq(start, end) => numBetweenKeys(start, end) }.toSeq

  private def computeSizes(key: Seq[Char], times: Int, cache: MMap[(String, Int), Long]): Long = {
    if (times == 0) return key.length
    if (cache.contains(key.mkString, times)) return cache((key.mkString, times))

    ('A' +: key).sliding(2).map { case Seq(from, to) =>
      dirBetweenKeys(from, to).map(key =>
        val size = computeSizes(key, times - 1, cache)
        cache += (((key.mkString, times - 1), size))
        size
      ).min
    }.sum
  }

  private def computeInputSize(key: String, times: Int): Long = {
    val first = computeFirstInputs(key, times)
    val cache = MMap[(String, Int), Long]()

    first.map(options => options.map(option => computeSizes(option, times - 1, cache)).min).sum
  }

  private def computeComplexity(key: String, times: Int): Long =
      key.toList.filter(_.isDigit).mkString.toInt * computeInputSize(key, times)

  @tailrec
  private def validMoves(moves: Seq[Char], start: (Int, Int), hole: (Int, Int)): Boolean = moves.map(Direction.fromChar).toList match
    case Seq() => true
    case m::ms => (m.dx, m.dy) + start != hole && validMoves(moves.tail, (m.dx, m.dy) + start, hole)

  override def partOne(keys: Seq[String]): Long = keys.map(key => computeComplexity(key, 3)).sum

  override def partTwo(keys: Seq[String]): Long = keys.map(key => computeComplexity(key, 26)).sum
}
