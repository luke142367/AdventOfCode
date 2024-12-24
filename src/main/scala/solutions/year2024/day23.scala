package solutions.year2024

import utils.Day
import utils.Year.Year24

import scala.collection.mutable.Map as MMap

object day23 extends Day[Map[String, Set[String]], Int, String](Year24, 23) {
  private val connectionRegex = "(\\w{2})-(\\w{2})".r

  def parseInput(input: String): Map[String, Set[String]] = {
    val connections = input.linesIterator.map(line =>
      val Seq(from, to) = connectionRegex.findFirstMatchIn(line).get.subgroups
      (from, to)
    ).toSeq
    val bothWays = connections ++ connections.map((a, b) => (b, a))
    bothWays.groupBy(_._1).mapValues(_.map(_._2).toSet).toMap
  }

  private def findTrios(conMap: Map[String, Set[String]]): Set[Set[String]] =
    conMap.keySet.flatMap(t =>
      val direct = conMap(t)
      direct.flatMap(d => conMap(d).intersect(direct).map(s => Set[String](t, d, s)))
    )

  def partOne(connections: Map[String, Set[String]]): Int = {
    val bothWays = connections ++ connections.map((a, b) => (b, a))
    val conMap = bothWays.groupBy(_._1).mapValues(_.map(_._2).toSet).toMap

    findTrios(connections).count(trio => trio.exists(_.head == 't'))
  }

  override def partTwo(connections: Map[String, Set[String]]): String = {
    val trios = findTrios(connections).toSeq
    
    // Count how often each node appears in the trios
    val occurrences = connections.keys.map(key => (key, trios.map(_.count(trio => trio.contains(key))).sum)).toMap
    
    // Score each trio based on the sum of the occurrences of each node
    val scoredTrios = trios.map(trio => (trio, trio.toSeq.map(t => occurrences(t)).sum))
    val highest = scoredTrios.maxBy(_._2)._2

    // Find the distinct nodes in the trios with the highest score
    val bestNodes = scoredTrios.filter(_._2 == highest).flatMap(_._1).distinct
    bestNodes.sorted.mkString(",")
  }
}
