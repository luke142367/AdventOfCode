package answers

import utils.FileHandler.readFile

case class File(size: Int, id: Int)

object dayNine {
  private val sample = "2333133121414131402"

  private val sample2 = "4083766302"

  private def parseInput(input: String): (Seq[File], Seq[Int]) = {
    val (files, spaces) = input.zipWithIndex.partition(_._2 % 2 == 0)
    (files.map(_._1).zipWithIndex.map((c, id) => File(c.toString.toInt, id)), spaces.map(_._1.toString.toInt))
  }

  private def compressFiles(files: List[File], reversedFiles: List[File], spaces: List[Int], fileSize: Long) : List[File] = (files, reversedFiles, spaces) match
    case (f::fs, r::rs, s::ss) if fileSize <= r.size => List(File(fileSize.toInt, f.id))
    case (f::fs, r::rs, s::ss) if s == 0 => f +: compressFiles(fs, r +: rs, ss, fileSize - f.size)
    case (f::fs, r::rs, s::ss) if r.size == s => f +: r +: compressFiles(fs, rs, ss, fileSize - f.size - r.size)
    case (f::fs, r::rs, s::ss) if r.size < s => f +: compressFiles(r +: fs, rs, (s - r.size) +: ss, fileSize - f.size)
    case (f::fs, r::rs, s::ss) if r.size > s =>
      f +: File(s, r.id) +: compressFiles(fs, File(r.size - s, r.id) +: rs, ss, fileSize - f.size - s)


  private def calculateChecksum(files: Seq[File], index: Long = 0): Long = files match
    case Seq() => 0
    case f::fs => (0 until f.size).map(i => (i + index) * f.id).sum + calculateChecksum(files.tail, index + f.size)

  private def partOne(files: Seq[File], spaces: Seq[Int]): Long = {
    val fileSize = files.map(_.size.toLong).sum

    val compressed = compressFiles(files.toList, files.reverse.toList, spaces.toList, fileSize)

    val check = compressed.groupBy(_.id).map((id, files) => File(files.map(_.size).sum, id)).toSeq.sortBy(_.id)

    calculateChecksum(compressed)
  }


  def main(args: Array[String]): Unit = {
    val input = readFile("day9.txt")

    val (files, spaces) = parseInput(input)

    val now = System.currentTimeMillis()
    val result = partOne(files.filter(_.size != 0), spaces)
    val taken = System.currentTimeMillis() - now

    println(result)
    println(s"Runtime: $taken ms")
  }
}
