package answers

import utils.FileHandler.readFile

import scala.annotation.tailrec

case class Guard(x: Int, y: Int, dx: Int, dy: Int)

object day14 {

  private val sample = """p=0,4 v=3,-3
                         |p=6,3 v=-1,-3
                         |p=10,3 v=-1,2
                         |p=2,0 v=2,-1
                         |p=0,0 v=1,3
                         |p=3,0 v=-2,-2
                         |p=7,6 v=-1,-3
                         |p=3,0 v=-1,-2
                         |p=9,3 v=2,3
                         |p=7,3 v=-1,2
                         |p=2,4 v=2,-3
                         |p=9,5 v=-3,-3""".stripMargin

  private val sampleHeight = 7
  private val sampleWidth = 11

  private val inputWidth = 101
  private val inputHeight = 103

  private val treeThreshold = 29

  private val inputRegex = "p=(\\d+),(\\d+) v=(-?\\d+),(-?\\d+)".r

  private def parseInput(input: String): Seq[Guard] = input
    .linesIterator
    .map(line =>
      val Seq(x, y, dx, dy) = inputRegex.findFirstMatchIn(line).get.subgroups.map(_.toInt)
      Guard(x, y, dx, dy)
    ).toSeq

  private def moveGuard(guard: Guard, seconds: Int, width: Int, height: Int): Guard = {
    val newX = (guard.x + guard.dx * seconds) % width
    val newY = (guard.y + guard.dy * seconds) % height
    Guard(if (newX < 0) width + newX else newX, if (newY < 0) height + newY else newY, guard.dx, guard.dy)
  }

  private def partOne(guards: Seq[Guard], width: Int, height: Int): Int = {
    val updated = guards.map(guard => moveGuard(guard, 100, width, height))

    val q1 = updated.count(guard => guard.x < width / 2 && guard.y < height / 2)
    val q2 = updated.count(guard => guard.x > width / 2 && guard.y < height / 2)
    val q3 = updated.count(guard => guard.x < width / 2 && guard.y > height / 2)
    val q4 = updated.count(guard => guard.x > width / 2 && guard.y > height / 2)

    q1 * q2 * q3 * q4
  }

  private def partTwo(guards: Seq[Guard], width: Int, height: Int, shouldRender: Boolean = false): Int = {
    val seconds = findWithClustering(guards, width, height, treeThreshold)
    if (shouldRender) {
      render(guards.map(g => moveGuard(g, seconds, width, height)), width, height)
    }
    seconds
  }

  @tailrec
  private def findWithClustering(guards: Seq[Guard], width: Int, height: Int, clustering: Double, seconds: Int = 0): Int = {
    val updated = guards.map(guard => moveGuard(guard, seconds, width, height))
    if (computeClustering(updated) < clustering) return seconds

    findWithClustering(guards, width, height, clustering, seconds + 1)
  }

  private def render(guards: Seq[Guard], width: Int, height: Int): Unit = {
    val pts = (0 until height).map(y => (0 until width).map(x => (x, y)))
    val output = pts.map(line => line.map((x, y) =>
      val count = guards.count(g => g.x == x && g.y == y)
      if (count > 0) count.toString else ".").mkString).mkString("\n")
    println(output)
  }

  private def computeClustering(guards: Seq[Guard]): Double = {
    val pts = guards.map(g => (g.x, g.y))
    val (ax, ay) = (guards.map(_.x).sum / guards.size, guards.map(_.y).sum / guards.size)

    val distances = pts.map((x, y) => Math.sqrt(Math.pow(x - ax, 2) + Math.pow(y - ay, 2)))
    distances.sum / distances.size
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("day14.txt")

    val guards = parseInput(input)

    val now = System.currentTimeMillis()
    val result = partTwo(guards, inputWidth, inputHeight, true)
    val taken = System.currentTimeMillis() - now

    println(s"Seconds: $result")
    println(s"Runtime: $taken ms")
  }
}
