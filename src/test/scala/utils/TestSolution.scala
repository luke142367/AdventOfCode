package utils

class TestSolution[A, B, C](solution: Day[A, B, C]) {
  def partOne(input: String) = solution.partOne(solution.parseInput(input))

  def partTwo(input: String) = solution.partTwo(solution.parseInput(input))
}
