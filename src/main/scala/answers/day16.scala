package answers

import answers.Direction.Up
import utils.FileHandler.readFile

import scala.collection.mutable.Map as MMap
import scala.collection.mutable.PriorityQueue as PQ

case class Position(x: Int, y: Int, dir: Direction) {
  def forward: Position = Position(x + dir.dx, y + dir.dy, dir)

  def turnRight: Position = Position(x, y, dir.turnRight)

  def turnLeft: Position = this.turnRight.turnRight.turnRight
}

case class WeightedPosition(position: Position, weight: Int, route: Seq[Position])

object day16 {
  enum Cell:
    case Wall
    case Empty
    case Start
    case End

  object Cell {
    def fromChar(c: Char): Cell = c match
      case '#' => Wall
      case '.' => Empty
      case 'S' => Start
      case 'E' => End
  }

  private val weightedOrdering: Ordering[WeightedPosition] = Ordering.by[WeightedPosition, Int](_.weight).reverse

  private val simple =
    """#######
      |#S....#
      |#.#.#.#
      |#.....#
      |#.###.#
      |#.....#
      |#...E.#
      |#######
      |""".stripMargin

  private val sample = """###############
                         |#.......#....E#
                         |#.#.###.#.###.#
                         |#.....#.#...#.#
                         |#.###.#####.#.#
                         |#.#.#.......#.#
                         |#.#.#####.###.#
                         |#...........#.#
                         |###.#.#####.#.#
                         |#...#.....#.#.#
                         |#.#.#.###.#.#.#
                         |#.....#...#.#.#
                         |#.###.#.#.#.#.#
                         |#S..#.....#...#
                         |###############""".stripMargin

  private val sample2 = """#################
                          |#...#...#...#..E#
                          |#.#.#.#.#.#.#.#.#
                          |#.#.#.#...#...#.#
                          |#.#.#.#.###.#.#.#
                          |#...#.#.#.....#.#
                          |#.#.#.#.#.#####.#
                          |#.#...#.#.#.....#
                          |#.#.#####.#.###.#
                          |#.#.#.......#...#
                          |#.#.###.#####.###
                          |#.#.#...#.....#.#
                          |#.#.#.#####.###.#
                          |#.#.#.........#.#
                          |#.#.#.#########.#
                          |#S#.............#
                          |#################""".stripMargin

  private def parseInput(input: String): Seq[Seq[Cell]] = input.linesIterator.map(_.toList.map(Cell.fromChar)).toSeq

  private def findStart(grid: Seq[Seq[Cell]]): Position = {
    val (x, y) = grid.indices.flatMap(y => grid(y).indices.flatMap(x => if (grid(y)(x) == Cell.Start) Some((x, y)) else None)).head
    Position(x, y, Direction.Right)
  }

  private def bfs(grid: Seq[Seq[Cell]]): Seq[WeightedPosition] = {
    val start = findStart(grid)
    val positions = PQ[WeightedPosition](WeightedPosition(start, 0, Seq(start)))(weightedOrdering)
    val visited = MMap[Position, Int]()

    while ({val closest = positions.head.position; grid(closest.y)(closest.x) != Cell.End}) {
      val current = positions.dequeue()

      if (!visited.contains(current.position) || visited(current.position) == current.weight) {
        visited.addOne((current.position, current.weight))

        val currentCell = grid(current.position.y)(current.position.x)

        if (currentCell != Cell.Wall) {
          positions.addAll(Seq(
            WeightedPosition(current.position.forward, current.weight + 1, current.position.forward +: current.route),
            WeightedPosition(current.position.turnRight.forward, current.weight + 1001, current.position.turnRight.forward +: current.route),
            WeightedPosition(current.position.turnLeft.forward, current.weight + 1001, current.position.turnLeft.forward +: current.route),
          ))
        }
      }
    }

    val lowest = positions.head.weight
    val end = positions.head.position
    positions.filter(pos => pos.weight == lowest && pos.position == end).toSeq
  }

  private def partOne(grid: Seq[Seq[Cell]]): Int = bfs(grid).head.weight

  private def partTwo(grid: Seq[Seq[Cell]]): Int = bfs(grid).flatMap(_.route.map(pos => (pos.x, pos.y))).toSet.size

  def main(args: Array[String]): Unit = {
    val input = readFile("day16.txt")

    val grid = parseInput(input)

    val resultOne = partOne(grid)
    val resultTwo = partTwo(grid)

    println(s"Result One: $resultOne")
    println(s"Result Two: $resultTwo")
  }
}
