package answers

import utils.FileHandler.readFile

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.PriorityQueue as PQ
import scala.collection.mutable.Set as MSet

object day18 {
  enum Cell:
    case Wall
    case Empty

  private case class WeightedPoint(point: (Int, Int), weight: Int)

  private val pointOrdering = Ordering.by[WeightedPoint, Int](_.weight).reverse

  private val sample = """5,4
                         |4,2
                         |4,5
                         |3,0
                         |2,1
                         |6,3
                         |2,4
                         |1,5
                         |0,6
                         |3,3
                         |2,6
                         |5,1
                         |1,2
                         |5,5
                         |2,5
                         |6,5
                         |1,4
                         |0,4
                         |6,4
                         |1,1
                         |6,1
                         |1,0
                         |0,5
                         |1,6
                         |2,0
                         |""".stripMargin

  private val sampleSize = 6
  private val inputSize = 70

  private val inputRegex = "(\\d+),(\\d+)".r

  private def parseInput(input: String): Seq[(Int, Int)] =
    input.linesIterator.map(line =>
      val List(x,y) = inputRegex.findFirstMatchIn(line).get.subgroups.map(_.toInt)
      (x, y)
    ).toSeq

  private def generateGrid(corruptions: Seq[(Int, Int)], size: Int): Seq[Seq[Cell]] =
    (0 to size).map(y => (0 to size).map(x => if (corruptions.contains((x, y))) Cell.Wall else Cell.Empty))

  private def adjacents(from: (Int, Int)): Seq[(Int, Int)] = from match
    case (x, y) => Seq((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))

  private def shortestPath(start: (Int, Int), end: (Int, Int), grid: Seq[Seq[Cell]]): Option[Int] = {
    val positions = PQ[WeightedPoint](WeightedPoint(start, 0))(pointOrdering)
    val visited = MSet[(Int, Int)]()

    while (positions.nonEmpty && positions.head.point != end) {
      val current = positions.dequeue()
      val (x, y) = current.point
      if (!visited.contains(current.point) && x >= 0 && y >= 0 && x < grid.size && y < grid.size && grid(y)(x) != Cell.Wall) {
        visited += current.point
        positions.addAll(adjacents(current.point).map(pt => WeightedPoint(pt, current.weight + 1)))
      }
    }

    positions.headOption.map(_.weight)
  }

  @tailrec
  private def findLast(corruptions: Seq[(Int, Int)], size: Int, pass: Int, fail: Int): Int = {
    val middle = pass + (fail - pass) / 2

    val grid = generateGrid(corruptions.take(middle), size)
    val shortest = shortestPath((0, 0), (size, size), grid)

    if (shortest.isDefined && middle + 1 == fail) return fail
    if (shortest.isEmpty && middle - 1 == pass) return middle

    if (shortest.isDefined) findLast(corruptions, size, middle, fail) else findLast(corruptions, size, pass, middle)
  }

  private def partOne(corruptions: Seq[(Int, Int)]): Int = {
    val grid = generateGrid(corruptions.take(inputSize), inputSize)
    shortestPath((0,0), (inputSize, inputSize), grid).get
  }

  private def partTwo(corruptions: Seq[(Int, Int)]): (Int, Int) = {
    val last = findLast(corruptions, inputSize, 0, corruptions.size)
    corruptions(last - 1)
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("day18.txt")
    val corruptions = parseInput(input)

    val resultOne = partOne(corruptions)
    val resultTwo = partTwo(corruptions)

    println(s"Result One: $resultOne")
    println(s"Result Two: $resultTwo")
  }
}
