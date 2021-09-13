package exercise.sed

case class SedOptionsOutput(completedLines:List[String],stackLines:List[String])
case class LineOutput(line:String,stackLine:List[String])

class EmptySedOptionsOutput() extends SedOptionsOutput(
  completedLines = List[String](),
  stackLines = List[String](),
) {}
