package exercise.sed

import scala.annotation.tailrec
import java.time.LocalDateTime

object SedService {

  def usage: String = {
    val usage =
      """
Usage: sed [OPTION]... {script-only-if-no-other-script} [input-file]...

  -n,
                 suppress automatic printing of pattern space
  -e script,
                 add the script to the commands to be executed
                 -- Allowed flags in -e option: p,g --
  -f script-file,
                 add the contents of script-file to the commands to be executed
  -i,
                 edit files in place
  -h,
                 display this help and exit

  """

    usage
  }

  def validateOptionsLength(map: SedOptions): Either[String, SedOptions] = {
    if (map.fOptions.length > 1)
      Left(s"The option -f should not appear at most once.")
    else if (map.nOptions.length > 1)
      Left(s"The option -n should not appear at most once.")
    else if (map.iOptions.length > 1)
      Left(s"The option -i should not appear at most once.")
    else if (map.eOptions.length < 1)
      Left(s"The option -i should not appear at least once.")
    else if (map.inputFile.length != 1)
      Left(s"An input file must be provided.")
    else Right(map)
  }

  @tailrec
  def parseOption(sedOptions: SedOptions, list: List[String]): Either[String, SedOptions] = {
    list match {
      case Nil => validateOptionsLength(sedOptions)
      case "-f" :: value :: tail =>
        val newSedOption = sedOptions.copy(fOptions = value :: sedOptions.fOptions)
        parseOption(newSedOption, tail)
      case "-e" :: value :: tail =>
        val newSedOption = sedOptions.copy(eOptions = value :: sedOptions.eOptions)
        parseOption(newSedOption, tail)
      case "-n" :: tail =>
        val newSedOption = sedOptions.copy(nOptions = true :: sedOptions.nOptions)
        parseOption(newSedOption, tail)
      case "-i" :: tail =>
        val newSedOption = sedOptions.copy(iOptions = true :: sedOptions.iOptions)
        parseOption(newSedOption, tail)
      case "-h" :: _ => Left(usage)
      case filename :: Nil =>
        val newSedOption = sedOptions.copy(inputFile = filename :: sedOptions.inputFile)
        parseOption(newSedOption, Nil)
      case option :: _ => Left(s"Unknown option: $option. \n $usage")
    }
  }

  @tailrec
  def executeManyReplacements(pendingCommands: List[Command], pendingLines: List[String], completedLines: List[String]): List[String] =
    pendingCommands match {
      case Nil => completedLines
      case command :: tail =>
        val replacedLines = executeReplacement(command: Command, pendingLines: List[String], List[String](), List[String]())
        this.executeManyReplacements(tail, replacedLines, replacedLines)
    }

  // TODO: return the shown lines
  @tailrec
  def executeReplacement(command: Command, pendingLines: List[String], completedLines: List[String], shownLines: List[String]): List[String] =
    pendingLines match {
      case Nil => completedLines
      case line :: tail =>
        val replacedLine = command.replaceInLine(line)

        if (command.flags.contains('p'))
          this.executeReplacement(command, tail, replacedLine :: completedLines, replacedLine :: shownLines)
        else
          this.executeReplacement(command, tail, replacedLine :: completedLines, line :: shownLines)
    }

  def executeOneCommand(commandString: String, filePath: String): Either[String, List[String]] = {
    FileManager.readFileByLines(filePath) match {
      case Some(contentLines) =>
        Command.validateCommand(commandString) match {
          case Right(command: Command) =>
            Right(this.executeReplacement(command, contentLines, List[String](), List[String]()))
          case Left(message: String) =>
            Left(message)
        }
      case None =>
        Left("There is no file content")
    }
  }

  def executeManyCommands(sedOptions: SedOptions): Either[List[String], String] = {
    val commandStrings = Command.getCommandStrings(sedOptions)

    val validatedCommands: List[Either[String, Command]] = commandStrings
      .map((commandString: String) => Command.validateCommand(commandString))

    val failures = validatedCommands collect { case Left(f) => f }
    val commands = validatedCommands collect { case Right(r) => r }

    if (failures.nonEmpty)
      return Left(failures)

    val filepath = sedOptions.inputFile.head

    val completedLines =
      FileManager.readFileByLines(filepath) match {
        case Some(contentLines) =>
          val completedLines = executeManyReplacements(commands, contentLines, List[String]())
          Right(completedLines)
        case None =>
          Left(List("There is no file content"))
      }

    completedLines match {
      case Right(lines) =>
        val data = lines.mkString("\n")
        val iOptions = sedOptions.iOptions
        val nOptions = sedOptions.nOptions

        if(iOptions.nonEmpty && iOptions.head) {

          FileManager.writeFile(s"${LocalDateTime.now()}-${filepath}",data)

          Right("")
        }else if (nOptions.nonEmpty && nOptions.head){
          Right(data) // print only the displayable changes
        }else{
          Right(data)
        }
      case Left(messages) => Left(messages)
    }


  }
}
