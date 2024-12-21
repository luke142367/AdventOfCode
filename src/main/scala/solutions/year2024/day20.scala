package solutions.year2024

import utils.Day
import utils.Year.Year24

import scala.collection.mutable
import scala.collection.mutable.PriorityQueue as PQ
import scala.collection.mutable.Set as MSet

object day20 extends Day[Seq[Seq[day20.Cell]], Int, Int](Year24, 20) {
  object Cell {
    def fromChar(char: Char): Cell = char match
      case '#' => Wall
      case '.' => Empty
      case 'S' => Start
      case 'E' => End
  }

  enum Cell:
    case Wall
    case Empty
    case Start
    case End

  private case class WeightedPoint(point: (Int, Int), weight: Int, path: Seq[(Int, Int)])

  private val pointOrdering = Ordering.by[WeightedPoint, Int](_.weight).reverse

  def parseInput(input: String): Seq[Seq[Cell]] = input.linesIterator.map(_.toList.map(Cell.fromChar)).toSeq

  private def find(grid: Seq[Seq[Cell]], target: Cell): (Int, Int) = {
    val (x, y) = grid.indices.flatMap(y => grid(y).indices.flatMap(x => if (grid(y)(x) == target) Some((x, y)) else None)).head
    (x, y)
  }

  private def shortestPath(grid: Seq[Seq[Cell]]): Seq[(Int, Int)] = {
    val start = find(grid, Cell.Start)
    val end = find(grid, Cell.End)
    val points = PQ[WeightedPoint](WeightedPoint(start, 0, Seq(start)))(pointOrdering)
    val visited = MSet[(Int, Int)]()

    while (points.head.point != end) {
      val current = points.dequeue()
      if (!visited.contains(current.point)) {
        visited += current.point
        val (x, y) = current.point

        if (grid(y)(x) != Cell.Wall) {
          points.addAll(
            current.point.adjacents(grid.size, grid.head.size).map(point =>
              WeightedPoint(point, current.weight + 1, point +: current.path)
            )
          )
        }
      }
    }

    points.head.path.reverse
  }

  private def findSkips(path: IndexedSeq[(Int, Int)], time: Int): Int = path.indices.map(index =>
      (index + 100 until path.size)
        .count(end =>
          val distance = path(index).distance(path(end))
          end - index - 100 >= distance && distance <= time
        )
    ).sum

  override def partOne(grid: Seq[Seq[Cell]]): Int = findSkips(shortestPath(grid).toIndexedSeq, 2)

  override def partTwo(grid: Seq[Seq[Cell]]): Int = findSkips(shortestPath(grid).toIndexedSeq, 20)
}
