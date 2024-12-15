package utils

import scala.io.Source.fromFile

object FileHandler {
  def readFile(fileName: String): String =
    fromFile("inputs/" + fileName) match
      case source => try source.mkString finally source.close()
}
