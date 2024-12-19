package solutions.year2024

import utils.{Day, Year}
import utils.Year.Year24

object day05 extends Day[(Seq[(Int, Int)], Seq[Seq[Int]]), Int, Int](Year24, 5) {
  private val orderRegex = "(\\d+)\\|(\\d+)".r

  private val sample: String = """47|53
                 |97|13
                 |97|61
                 |97|47
                 |75|29
                 |61|13
                 |75|53
                 |29|13
                 |97|29
                 |53|29
                 |61|53
                 |97|53
                 |61|29
                 |47|13
                 |75|47
                 |97|75
                 |47|61
                 |75|61
                 |47|29
                 |75|13
                 |53|13
                 |
                 |75,47,61,53,29
                 |97,61,53,29,13
                 |75,29,13
                 |75,97,47,61,53
                 |61,13,29
                 |97,13,75,29,47""".stripMargin

  private def buildCorrectOrdering(orders: Seq[(Int, Int)], pages: Set[Int]): Seq[Int] = {
    val relevantOrders = orders.filter((a,b) => pages.contains(a) && pages.contains(b))

    pages.toSeq.sortBy(x => relevantOrders.count(_._1 == x)).reverse
  }

  private def middlePage(pages: Seq[Int]): Int = pages(pages.size / 2)

  private def calculate(orders: Seq[(Int, Int)], pages: Seq[Seq[Int]], comparator: ((Seq[Int], Seq[Int])) => Boolean): Int =
    pages
      .map(ps => buildCorrectOrdering(orders, ps.toSet))
      .zip(pages)
      .filter(comparator)
      .map(_._1)
      .map(middlePage)
      .sum

  override def partOne(input: (Seq[(Int, Int)], Seq[Seq[Int]])): Int = input match
    case (orders, pages) => calculate(orders, pages, (a, b) => a == b)

  override def partTwo(input: (Seq[(Int, Int)], Seq[Seq[Int]])): Int = input match
    case (orders, pages) => calculate(orders, pages, (a, b) => a != b)

  def parseInput(input: String): (Seq[(Int, Int)], Seq[Seq[Int]]) = {
    val orderPairs = orderRegex.findAllMatchIn(input).map(m => (m.group(1).toInt, m.group(2).toInt)).toSeq
    val pageSets = input.linesIterator.toSeq.filter(_.contains(",")).map(_.split(",").toSeq).map(_.map(_.toInt))
    (orderPairs, pageSets)
  }
}
