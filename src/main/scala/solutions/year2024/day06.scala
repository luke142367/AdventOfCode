package solutions.year2024

import utils.Year.Year24
import utils.{Day, Year}

import scala.annotation.tailrec

case class LoopException() extends Exception("Loop!!!")

enum Direction(val dx: Int, val dy: Int):
  def turnRight: Direction = this match
    case Up => Right
    case Down => Left
    case Right => Down
    case Left => Up

  case Up extends Direction(0, -1)
  case Down extends Direction(0, 1)
  case Right extends Direction(1, 0)
  case Left extends Direction(-1, 0)

object Direction {
  def fromChar(c: Char): Direction = c match
    case '^' => Direction.Up
    case '>' => Direction.Right
    case '<' => Direction.Left
    case 'v' => Direction.Down
}

enum Cell:
  case Open extends Cell
  case Wall extends Cell

object Cell {
  def fromChar(char: Char): Cell = char match
    case '.' | '^' => Open
    case '#' => Wall
    case _ => throw Exception("wtf!!!")
}

case class Location(x: Int, y: Int, direction: Direction)

object day06 extends Day[(Seq[Seq[Cell]], Location), Int, Int](Year24, 6) {
  private val sample = """....#.....
                         |.........#
                         |..........
                         |..#.......
                         |.......#..
                         |..........
                         |.#..^.....
                         |........#.
                         |#.........
                         |......#...""".stripMargin


  def parseInput(input: String): (Seq[Seq[Cell]], Location) = {
    val grid = input.linesIterator.map(line => line.toList.map(Cell.fromChar)).toSeq
    val (startingLine, y) = input.linesIterator.zipWithIndex.find(_._1.contains('^')).get
    val x = startingLine.zipWithIndex.find(_._1 == '^').get._2

    (grid, Location(x, y, Direction.Up))
  }

  def parseFollowed(input: String): Set[(Int, Int)] = {
    input.linesIterator.zipWithIndex.flatMap((line, y) =>
      line.zipWithIndex.filter(_._1 == 'X').map(_._2).map(x => (x, y))
    ).toSet
  }

  private def checkFinished(grid: Seq[Seq[Cell]], start: Location): Boolean = start.direction match
    case Direction.Up => start.y == 0
    case Direction.Down => start.y == grid.size - 1
    case Direction.Right => start.x == grid.head.size - 1
    case Direction.Left => start.x == 0

  private def addWall(grid: Seq[Seq[Cell]], wallX: Int, wallY: Int): Seq[Seq[Cell]] = {
    val newRow = grid(wallY).take(wallX) ++ (Cell.Wall +: grid(wallY).drop(wallX + 1))
    grid.take(wallY) ++ (newRow +: grid.drop(wallY + 1))
  }

  @tailrec
  private def computePath(grid: Seq[Seq[Cell]], start: Location, previousPath: Seq[Location] = Seq()): Seq[Location] = {
    if (previousPath.contains(start)) throw LoopException()

    val newPath = start +: previousPath

    if (checkFinished(grid, start)) return newPath

    val newLocation = {
      val (newX, newY) = (start.x + start.direction.dx, start.y + start.direction.dy)
      grid(newY)(newX) match
        case Cell.Open => Location(newX, newY, start.direction)
        case Cell.Wall => Location(start.x, start.y, start.direction.turnRight)
    }

    computePath(grid, newLocation, newPath)
  }

  override def partOne(input: (Seq[Seq[Cell]], Location)): Int = input match
    case (grid, start) => computePath(grid, start).distinctBy(loc => (loc.x, loc.y)).size

  override def partTwo(input: (Seq[Seq[Cell]], Location)): Int = computePath(input._1, input._2) match
    case path => getLoops(input._1, path.tail, path.tail.head)

  private def getLoops(grid: Seq[Seq[Cell]], path: Seq[Location], start: Location): Int = {
    if (path.isEmpty) return 0

    val (x, y) = (path.head.x, path.head.y)
    val loop = try {
      computePath(addWall(grid, x, y), start, path)
      0
    } catch {
      case _: LoopException => 1
    }
    val nextPath = path.dropWhile(loc => (loc.x, loc.y) == (x, y))
    loop + getLoops(grid, nextPath, path.head)
  }
}
