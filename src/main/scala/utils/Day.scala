package utils

enum Year(val name: String):
  case Year24 extends Year("2024")
  case Year17 extends Year("2017")

abstract class Day[A, B, C](year: Year, day: Int) {
  private def filePath : String = s"year${year.name}/day$day.txt"

  private def readFile: String = FileHandler.readFile(filePath)

  def parseInput(input: String): A

  def partOne(input: A): B

  def partTwo(input: A): C = ???

  def main(args: Array[String]): Unit = {
    println(s"Running: Year ${year.name} / Day $day")

    val (runtimeOne, resultOne) = timed { partOne(parseInput(readFile)) }
    println(s"Result One: $resultOne Runtime: ${runtimeOne}ms")

    try {
      val (runtimeTwo, resultTwo) = timed { partTwo(parseInput(readFile)) }
      println(s"Result Two: $resultTwo Runtime: ${runtimeTwo}ms")
    } catch
      case _: NotImplementedError => println("Result Two: NA")
  }
}

def timed[A](fn: => A): (Long, A) = {
  val start = System.currentTimeMillis()
  val result = fn
  val end = System.currentTimeMillis()

  (end - start, result)
}