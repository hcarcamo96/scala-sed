package exercise.sed

import java.io.{File, FileNotFoundException, IOException, PrintWriter}
import scala.io.Source

object FileManager {
  // Returns the file data as String
  def readFile(pathName: String): Option[String] = {
    try {
      val sourceFile = Source.fromFile(pathName)
      val sourceStr = sourceFile.mkString
      //sourceFile.close()
      Some(sourceStr)
    } catch {
      case _: FileNotFoundException =>
        println("Couldn't find that file.")
        None
      case _: IOException =>
        println("Had an IOException trying to read that file")
        None
    }
  }

  // Returns the file data as String Iterator
  def readFileByLines(pathName: String): Option[List[String]] =
    try {
      val sourceFile = Source.fromFile(pathName)
      val sourceLines = sourceFile.getLines().toList
      sourceFile.close()
      Some(sourceLines)
    } catch {
      case _: FileNotFoundException =>
        println("Couldn't find that file.")
        None
      case e: IOException =>
        println(e)
        println("Had an IOException trying to read that file")
        None
    }

  def writeFile(pathName: String, data: String): Unit =
    try {
      val writer = new PrintWriter(new File(pathName))

      writer.write(data)
      writer.close()
    } catch {
      case _: FileNotFoundException =>
        println("Couldn't find that file.")
      case _: IOException =>
        println("Had an IOException trying to read that file")
    }

}
