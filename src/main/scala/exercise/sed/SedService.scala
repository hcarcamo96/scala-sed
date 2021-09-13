package exercise.sed

import exercise.sed.commands.{Command, FileManager}
import exercise.sed.options.{EmptySedOptionsOutput, LineOutput, SedOptions, SedOptionsOutput}

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
  def executeManyReplacements(commands: List[Command], pendingLines: List[String],sedOptionOutput: SedOptionsOutput): SedOptionsOutput =
    pendingLines match {
      case Nil => sedOptionOutput
      case line :: tail =>
        val lineOutput = executeReplacement(commands, line, List[String]())
        val completedLines  = lineOutput.line :: sedOptionOutput.completedLines
        val stackLines = lineOutput.stackLine.reverse ++ sedOptionOutput.stackLines
        this.executeManyReplacements(commands, tail, SedOptionsOutput(completedLines,stackLines))
    }

  @tailrec
  def executeReplacement(pendingCommands: List[Command], line:String, stackLine:List[String]): LineOutput =
    pendingCommands match {
      case Nil => LineOutput(line, stackLine)
      case command :: tail =>
        val replacedLine = command.replaceInLine(line)

        if (command.flags.contains('p') && replacedLine != line)
          this.executeReplacement(tail, replacedLine, replacedLine :: stackLine)
        else
          this.executeReplacement(tail, replacedLine, stackLine)
    }

  def executeOneCommand(commandString: String, filePath: String): Option[SedOptionsOutput] = {
    FileManager.readFileByLines(filePath) match {
      case Some(contentLines) =>
        Command.validateCommand(commandString) match {
          case Right(command: Command) =>
            val sedOptionOutput = new EmptySedOptionsOutput()
            val replacementsOutput = this.executeManyReplacements(List(command), contentLines,sedOptionOutput)
            Some(replacementsOutput)
          case Left(message: String) =>
            println(message)
            None
        }
      case None =>
        println("There is no file content")
        None
    }
  }

  def manageOutput(executionOutput: Option[SedOptionsOutput], sedOptions: SedOptions):Option[String] = {
    val filepath = sedOptions.inputFile.head

    executionOutput match {
      case Some(changedLines) =>

        val completedLinesData = changedLines.completedLines.reverse.mkString("\n")
        val stackLinesData = changedLines.stackLines.reverse.mkString("\n")
        val iOptions = sedOptions.iOptions
        val nOptions = sedOptions.nOptions

        if(iOptions.nonEmpty && iOptions.head) {
          FileManager.writeFile(s"${LocalDateTime.now()}-${filepath}",completedLinesData)
          Some("")
        }else if (nOptions.nonEmpty && nOptions.head){
          Some(stackLinesData)
        }else {
          Some(completedLinesData)
        }
      case None =>
        None
    }
  }

  def executeManyCommands(sedOptions: SedOptions): Option[String] = {
    val commandStrings = Command.getCommandStrings(sedOptions)

    val validatedCommands: List[Either[String, Command]] = commandStrings
      .map((commandString: String) => Command.validateCommand(commandString))

    val failures = validatedCommands collect { case Left(f) => f }
    val commands = validatedCommands collect { case Right(r) => r }

    if (failures.nonEmpty) {
      println(failures)
      return None
    }

    val filepath = sedOptions.inputFile.head

    val executionOutput =
      FileManager.readFileByLines(filepath) match {
        case Some(contentLines) =>
          val sedOptionOutput = new EmptySedOptionsOutput()
          val replacementsOutput = executeManyReplacements(commands, contentLines, sedOptionOutput)
          Some(replacementsOutput)
        case None =>
          println("There is no file content")
          None
      }

    this.manageOutput(executionOutput,sedOptions)
  }
}
