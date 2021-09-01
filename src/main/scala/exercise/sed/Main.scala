package exercise.sed

object Main extends App {
  val usage = SedService.getUsage()
  val initialStates = SedService.getInitialStates()
  val argList = args.toList.slice(1, args.length)

  val result = argList match {
    case commandString :: filepath :: Nil =>
      Command.validateCommand(commandString) match {
        case Right(command:Command) =>
          Right(SedService.applyCommand(command,filepath))
        case Left(message:String) =>
          Left(message)
    }
//    case _ :: _ :: tail =>
//      SedService.parseOption(initialStates, argList)
    case _ => Left(usage)
  }

  println(result)

}
