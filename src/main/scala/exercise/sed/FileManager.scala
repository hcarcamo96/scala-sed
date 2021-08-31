package exercise.sed

import java.io.{File, PrintWriter}
import scala.io.Source

object FileManager {
  // Returns the file data as String
  def readFile(pathName: String): String =
    Source.fromFile(pathName).mkString

  // Returns the file data as String Iterator
  def readFileByLines(pathName: String): Iterator[String] =
    Source.fromFile(pathName).getLines()

  def writeFile(pathName: String, data: String): Unit = {
    val writer = new PrintWriter(new File(pathName))

    writer.write(data)
    writer.close()
  }

  def validateFilePath(filePath:String) = ???
}
