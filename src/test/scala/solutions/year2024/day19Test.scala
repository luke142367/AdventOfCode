package solutions.year2024

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import utils.TestSolution

class day19Test extends AnyWordSpec {
  private val solution = TestSolution(day19)

  private val sample = """r, wr, b, g, bwu, rb, gb, br
                         |
                         |brwrr
                         |bggr
                         |gbbr
                         |rrbgbr
                         |ubwu
                         |bwurrg
                         |brgr
                         |bbrgwb""".stripMargin

  "part one" should {
    "handle sample" in {
      solution.partOne(sample) shouldBe 6
    }
  }

  "part two" should {
    "handle sample" in {
      solution.partTwo(sample) shouldBe 16
    }
  }
}
