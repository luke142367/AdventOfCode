package answers

import utils.FileHandler.readFile

case class Antenna(x: Int, y: Int, char: Char)

object dayEight {

  private val sample = """............
                         |........0...
                         |.....0......
                         |.......0....
                         |....0.......
                         |......A.....
                         |............
                         |............
                         |........A...
                         |.........A..
                         |............
                         |............""".stripMargin

  private val sample2 = """T....#....
                          |...T......
                          |.T....#...
                          |.........#
                          |..#.......
                          |..........
                          |...#......
                          |..........
                          |....#.....
                          |..........""".stripMargin

  private def parseInput(input: String): (Int, Int, Seq[Antenna]) = {
    val antennas = input.linesIterator.zipWithIndex.flatMap((line, y) =>
      line.zipWithIndex.filter((c, x) => c != '.' && c != '#').map((c, x) => Antenna(x, y, c))
    ).toSeq

    (input.linesIterator.size, input.linesIterator.toSeq.head.length, antennas)
  }

  private def computePairAntinodes(first: Antenna, second: Antenna): Set[(Int, Int)] = {
    val dx = first.x - second.x
    val dy = first.y - second.y

    Set((first.x + dx, first.y + dy), (second.x - dx, second.y - dy))
  }

  private def computeLineAntinodes(x: Int, y: Int, dx: Int, dy: Int, height: Int, width: Int): Seq[(Int, Int)] = {
    if (x < 0 || x >= width || y < 0 || y >= height) return Seq()

    (x, y) +: computeLineAntinodes(x + dx, y + dy, dx, dy, height, width)
  }

  private def computePairAntinodes2(first: Antenna, second: Antenna, height: Int, width: Int): Seq[(Int, Int)] = {
    val dx = first.x - second.x
    val dy = first.y - second.y

    computeLineAntinodes(first.x, first.y, dx, dy, height, width) ++
      computeLineAntinodes(second.x, second.y, -dx, -dy, height, width)
  }

  private def computeSignalAntinodes(antennas: Seq[Antenna]): Set[(Int, Int)] = {
    antennas.flatMap(first =>
      antennas.filter(_ != first).map(second =>
        computePairAntinodes(first, second)
      )
    ).reduce(_ ++ _)
  }

  private def computeSignalAntinodes2(antennas: Seq[Antenna], height: Int, width: Int): Set[(Int, Int)] = {
    antennas.flatMap(first =>
      antennas.filter(_ != first).flatMap(second =>
        computePairAntinodes2(first, second, height, width)
      ).toSet
    ).toSet
  }

  private def partOne(height: Int, width: Int, antennas: Seq[Antenna]): Int = antennas
    .groupBy(_.char)
    .values
    .map(computeSignalAntinodes).reduce(_ ++ _)
    .count((x, y) => x >= 0 && x < width && y >= 0 && y < height)

  private def partTwo(height: Int, width: Int, antennas: Seq[Antenna]): Int = {
      antennas.groupBy(_.char).values.map(ants =>
        computeSignalAntinodes2(ants, height, width)
      ).reduce(_ ++ _).size
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("day8.txt")

    val (height, width, antennas) = parseInput(input)

    println(partTwo(height, width, antennas))
  }
}
