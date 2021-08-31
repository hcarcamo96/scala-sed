package exercise.sed


class Command(
               operation: String,
               regexp: String,
               replacement: String,
               flags: String
             )


object Command {
  def validateCommand (command: String): Either[String, Command] = {
    command.trim().split('/').toList match {
      case operation :: regexp :: replacement :: flags :: Nil =>
        if (operation != "s")
          Left("The allowed operation is 's' only")
        else if (validateFlags(flags))
          Left(s"There are duplicated or not allowed flags in $flags")
        else Right(new Command(operation,regexp,replacement,flags))
      case _ =>
        Left(s"The command $command must have s/[regexp]/[replacement]/[p,g,i] ")
    }
  }

  def validateFlags(flags:String):Boolean ={
    val duplicatePattern = "/(\\w)(?=.+\\1)/g".r
    val notFlagsPattern = "/[^g,i,p,]/g".r
    flags match {
      case duplicatePattern(c) => false
      case notFlagsPattern(c) => false
      case _ => true
    }
  }
}

