package exercise.sed


class Command(val operation: String, val regexp: String, val replacement: String, val flags: String) {
  def replaceInLine(line: String): String =
    if (this.flags.contains('g'))
      line.replaceAll(this.regexp, this.replacement)
    else
      line.replaceFirst(this.regexp, this.replacement)
}


object Command {
  def validateCommand(command: String): Either[String, Command] = {
    command.trim().split('/').toList match {
      case operation :: regexp :: replacement :: flags :: Nil =>
        println(flags)
        if (operation != "s")
          Left("The only allowed operation is 's'")
        else if (invalidFlags(flags))
          Left(s"There are duplicated or not allowed flags in $flags")
        else Right(new Command(operation, regexp, replacement, flags))
      case _ =>
        Left(s"The command $command must have s/[regexp]/[replacement]/[p|g|i|] ")
    }
  }

  def invalidFlags(flags: String): Boolean = {
    val duplicatePattern = "/(.)\\1+/g".r
    val notFlagsPattern = "/[^g,i,p,]/g".r
    val validationResult = duplicatePattern.matches(flags) || notFlagsPattern.matches(flags)

    validationResult
  }
}

