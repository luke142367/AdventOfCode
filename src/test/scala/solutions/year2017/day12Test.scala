package solutions.year2017

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import utils.TestSolution

class day12Test extends AnyWordSpec {
  private val solution = TestSolution(day12)

  private val sample = """0 <-> 2
                         |1 <-> 1
                         |2 <-> 0, 3, 4
                         |3 <-> 2, 4
                         |4 <-> 2, 3, 6
                         |5 <-> 6
                         |6 <-> 4, 5""".stripMargin

    "part one" should {
      "handle sample" in {
        solution.partOne(sample) shouldBe 6
      }
    }
}
