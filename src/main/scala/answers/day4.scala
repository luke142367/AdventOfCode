package answers

import utils.FileHandler.readFile

import scala.util.matching.Regex

object day4 {
  private val xmasSearch = "XMAS".r
  private val samxSearch = "SAMX".r

  private val masses = Set("MAS", "SAM")

  private def countOccurrences(input: String, term: Regex): Int = term.findAllMatchIn(input).size

  private def countXmas(input: String) = countOccurrences(input, xmasSearch) + countOccurrences(input, samxSearch)

  private def partOne(grid: Seq[Seq[Char]]): Int = {
    val horizontals = grid
    val verticals = grid.transpose
    val diagRight = diagonals(grid)
    val diagLeft = diagonals(grid.map(_.reverse))

    (horizontals ++ verticals ++ diagRight ++ diagLeft).map(line => countXmas(line.mkString)).sum
  }

  private def partTwo(grid: Seq[Seq[Char]]): Int = slidingWindow(grid).count(hasCrossMas)

  private def hasCrossMas(grid: Seq[Seq[Char]]): Boolean = {
    (masses contains diagonals(grid).filter(_.size == 3).head.mkString) &&
    (masses contains diagonals(grid.map(_.reverse)).filter(_.size == 3).head.mkString)
  }

  private def diagonals[A](grid: Seq[Seq[A]]): Seq[Seq[A]] =
    grid
      .zipWithIndex
      .map((line: Seq[A], shift: Int) => rotate(line, shift))
      .transpose
      .zipWithIndex
      .flatMap((line: Seq[A], shift: Int) => Seq(line.takeRight(shift), line.dropRight(shift)))
      .filter(_.nonEmpty)

  private def rotate[A](seq: Seq[A], shift: Int): Seq[A] = seq.drop(shift) ++ seq.take(shift)

  private def slidingWindow[A](grid: Seq[Seq[A]], size: Int = 3) =
    grid.map(_.sliding(size).toSeq).transpose.flatMap(_.sliding(size).toSeq)

  private val sample = """MMMSXXMASM
                         |MSAMXMSMSA
                         |AMXSXMAAMM
                         |MSAMASMSMX
                         |XMASAMXAMM
                         |XXAMMXXAMA
                         |SMSMSASXSS
                         |SAXAMASAAA
                         |MAMMMXMMMM
                         |MXMXAXMASX""".stripMargin

  private def parseInput(input: String): Seq[Seq[Char]] = input.lines.toSeq.map(_.toList)

  def main(args: Array[String]): Unit = {
    val input = readFile("day4.txt")
    val now = System.currentTimeMillis()
    val count = partOne(parseInput(input))
    val taken = System.currentTimeMillis() - now
    println(count)
    println(taken)
  }
}
