package exercise.sed

import exercise.sed.options.EmptySedOptions

object Main extends App {
  val usage = SedService.usage
  val argList = args.toList.slice(1, args.length)

  val result = argList match {
    case commandString :: filepath :: Nil =>
      SedService.executeOneCommand(commandString, filepath)
    case _ :: _ :: tail =>
      SedService.parseOption(new EmptySedOptions(), argList) match {
        case Right(optionsMap) =>
          SedService.executeManyCommands(optionsMap)
        case Left(message) => Left(message)
      }
    case _ => Left(usage)
  }

  println(result)

}
