package exercise.sed

import scala.annotation.tailrec

object SedService {
  type OptionMap = Map[String, List[Any]]

  def getUsage(): String = {
    val usage =
      """
Usage: sed [OPTION]... {script-only-if-no-other-script} [input-file]...

  -n,
                 suppress automatic printing of pattern space
  -e script,
                 add the script to the commands to be executed
                 -- Allowed flags in -e option: p,g,i --
  -f script-file,
                 add the contents of script-file to the commands to be executed
  -i,
                 edit files in place
  -h,
                 display this help and exit


GNU sed home page: <http://www.gnu.org/software/sed/>.
General help using GNU software: <http://www.gnu.org/gethelp/>.
E-mail bug reports to: <bug-sed@gnu.org>.

  """

    usage
  }

  def getInitialStates(): OptionMap = {
    Map(
      "f" -> List(),
      "e" -> List(),
      "n" -> List(),
      "i" -> List(),
      "inputFile" -> List()
    )
  }

  def validateOptionsLength(map: OptionMap): Either[String, OptionMap] = {
    if (map("f").length > 1)
      Left(s"The option -f should not appear at most once.")
    else if (map("n").length > 1)
      Left(s"The option -n should not appear at most once.")
    else if (map("i").length > 1)
      Left(s"The option -i should not appear at most once.")
    else if (map("e").length < 1)
      Left(s"The option -i should not appear at least once.")
    else if (map("inputFile").length != 1)
      Left(s"An input file must be provided.")
    else Right(map)
  }

  @tailrec
  def parseOption(map: OptionMap, list: List[String]): Either[String, OptionMap] = {
    list match {
      case Nil => validateOptionsLength(map)
      case "-f" :: value :: tail =>
        parseOption(map ++ Map("f" -> (value :: map("f"))), tail)
      case "-e" :: value :: tail =>
        parseOption(map ++ Map("e" -> (value :: map("e"))), tail)
      case "-n" :: tail =>
        parseOption(map ++ Map("n" -> (true :: map("n"))), tail)
      case "-i" :: tail =>
        parseOption(map ++ Map("i" -> (true :: map("i"))), tail)
      case "-h" :: _ => Left(getUsage())
      case filename :: Nil =>
        parseOption(map ++ Map("inputFile" -> (filename :: map("inputFile"))), Nil)
      case option :: _ => Left(s"Unknown option: $option. \n ${getUsage()}")

    }
  }

  def applyCommand(command: Command, inputFile: String): Option[List[String]] = {
    FileManager.readFileByLines(inputFile) match {
      case Some(contentIterator) =>
        val contentLines = contentIterator.toList
        val completedLines = this.executeReplacement(contentLines, command, List[String](), List[String]())
        Some(completedLines)
      case None => None
    }
  }

  // TODO: return the shown lines
  @tailrec
  def executeReplacement(pendingLines: List[String], command: Command, completedLines: List[String], shownLines: List[String]): List[String] =
    pendingLines match {
      case Nil => completedLines
      case line :: tail =>
        val replacedLine = command.replaceInLine(line)

        if (command.flags.contains('p'))
          this.executeReplacement(tail, command, replacedLine :: completedLines, replacedLine :: shownLines)
        else
          this.executeReplacement(tail, command, replacedLine :: completedLines, line :: shownLines)
    }
}
