package solutions.year2024

import utils.Day
import utils.Year.Year24

import scala.annotation.tailrec
import scala.collection.mutable.ArraySeq as MSeq
import scala.collection.mutable.Set as MSet

enum Space:
  def toChar: Char = this match
    case Wall => '#'
    case Box => 'O'
    case Empty => '.'
    case Robot => '@'
    case BoxLeft => '['
    case BoxRight => ']'

  def widened: Seq[Space] = this match
    case Box => Seq(BoxLeft, BoxRight)
    case Robot => Seq(Robot, Empty)
    case other => Seq(other, other)

  def toOther: Int = this match
    case BoxLeft => 1
    case BoxRight => -1
    case _ => throw Exception("Operation not supported")

  case Wall
  case Box
  case BoxLeft
  case BoxRight
  case Empty
  case Robot

object Space {
  def fromChar(c: Char): Space = c match
    case '#' => Space.Wall
    case 'O' => Space.Box
    case '.' => Space.Empty
    case '@' => Space.Robot
}

object day15 extends Day[(MSeq[MSeq[Space]], Seq[Direction]), Int, Int](Year24, 15) {
  private val sample = """##########
                         |#..O..O.O#
                         |#......O.#
                         |#.OO..O.O#
                         |#..O@..O.#
                         |#O#..O...#
                         |#O..O..O.#
                         |#.OO.O.OO#
                         |#....O...#
                         |##########
                         |
                         |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
                         |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
                         |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
                         |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
                         |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
                         |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
                         |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
                         |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
                         |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
                         |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".stripMargin

  private val sample2 = """########
                          |#..O.O.#
                          |##@.O..#
                          |#...O..#
                          |#.#.O..#
                          |#...O..#
                          |#......#
                          |########
                          |
                          |<^^>>>vv<v>>v<<""".stripMargin

  private val sample3 = """#######
                          |#...#.#
                          |#.....#
                          |#..OO@#
                          |#..O..#
                          |#.....#
                          |#######
                          |
                          |<vv<<^^<<^^""".stripMargin

  def parseInput(input: String): (MSeq[MSeq[Space]], Seq[Direction]) = {
    val Seq(gridInput, movesInput) = input.split("\n\n").toSeq
    val moves = movesInput.filter(_ != '\n').map(Direction.fromChar)
    val grid = MSeq(gridInput.linesIterator.map(line => MSeq(line.toList.map(Space.fromChar):_*)).toSeq:_*)

    (grid, moves)
  }

  private def findRobot(grid: MSeq[MSeq[Space]]): (Int, Int) =
    grid.indices.flatMap(y => grid(y).indices.flatMap(x => if (grid(y)(x) == Space.Robot) Some((x, y)) else None)).head


  @tailrec
  private def boxesEnd(grid: MSeq[MSeq[Space]], x: Int, y: Int, direction: Direction): (Int, Int) = grid(y)(x) match
    case Space.Box => boxesEnd(grid, x + direction.dx, y + direction.dy, direction)
    case _ => (x, y)

  private def canMoveBoxes(grid: MSeq[MSeq[Space]], x: Int, y: Int, direction: Direction): Boolean = grid(y)(x) match
    case Space.Wall => false
    case Space.Empty | Space.Robot => true
    case box => (box, direction) match
      case (_, Direction.Left) | (_, Direction.Right) => canMoveBoxes(grid, x + direction.dx, y, direction)
      case (Space.BoxLeft, _) | (Space.BoxRight, _) =>
        canMoveBoxes(grid, x, y + direction.dy, direction) && canMoveBoxes(grid, x + box.toOther, y + direction.dy, direction)

  // Assumes that boxes are movable
  private def moveBoxes(grid: MSeq[MSeq[Space]], x: Int, y: Int, direction: Direction): Unit = {
    grid(y)(x) match
      case Space.Wall => throw Exception("Boxes weren't movable :/")
      case Space.Empty | Space.Robot =>
      case box =>
        moveBoxes(grid, x + direction.dx, y + direction.dy, direction)
        grid(y + direction.dy)(x + direction.dx) = grid(y)(x)
        grid(y)(x) = Space.Empty
        if (direction == Direction.Up || direction == Direction.Down) {
          moveBoxes(grid, x + box.toOther, y + direction.dy, direction)
          grid(y + direction.dy)(x + box.toOther) = grid(y)(x + box.toOther)
          grid(y)(x + box.toOther) = Space.Empty
        }
  }

  @tailrec
  private def updateGrid(grid: MSeq[MSeq[Space]], x: Int, y: Int, moves: Seq[Direction]): Unit = {
    if (moves.isEmpty) return

    val move = moves.head
    val (newX, newY) = (x + move.dx, y + move.dy)
    val (endX, endY) = boxesEnd(grid, newX, newY, move)

    if (grid(endY)(endX) != Space.Wall) {
      grid(endY)(endX) = grid(newY)(newX)
      grid(newY)(newX) = Space.Empty
      updateGrid(grid, newX, newY, moves.tail)
    } else {
      updateGrid(grid, x, y, moves.tail)
    }
  }

  @tailrec
  private def updateGrid2(grid: MSeq[MSeq[Space]], x: Int, y: Int, moves: Seq[Direction]): Unit = {
    if (moves.isEmpty) return

    val move = moves.head
    val (newX, newY) = (x + move.dx, y + move.dy)

    if (canMoveBoxes(grid, newX, newY, move)) {
      moveBoxes(grid, newX, newY, move)
      grid(newY)(newX) = Space.Robot
      grid(y)(x) = Space.Empty
      updateGrid2(grid, newX, newY, moves.tail)
    } else {
      updateGrid2(grid, x, y, moves.tail)
    }
  }

  private def calculateGPS(grid: MSeq[MSeq[Space]]): Int =
    grid.indices.map(y => grid(y).indices.map(x =>
      if (Set(Space.Box, Space.BoxLeft).contains(grid(y)(x))) 100 * y + x else 0
    ).sum).sum

  private def render(grid : MSeq[MSeq[Space]]): Unit =
    println(grid.map(row => row.map(_.toChar).mkString).mkString("\n"))

  override def partOne(input: (MSeq[MSeq[Space]], Seq[Direction])): Int = input match
    case (grid: MSeq[MSeq[Space]], moves: Seq[Direction]) =>
      val (x, y) = findRobot(grid)
      updateGrid(grid, x, y, moves)
      calculateGPS(grid)

  override def partTwo(input: (MSeq[MSeq[Space]], Seq[Direction])): Int = input match
    case (grid: MSeq[MSeq[Space]], moves: Seq[Direction]) =>
      val widenedGrid = grid.map(row => row.flatMap(_.widened))
      val (x, y) = findRobot(widenedGrid)
      updateGrid2(widenedGrid, x, y, moves)
      calculateGPS(widenedGrid)
}
