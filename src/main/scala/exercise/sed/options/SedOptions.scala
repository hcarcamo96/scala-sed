package exercise.sed.options

case class SedOptions(
               fOptions: List[String],
               eOptions: List[String],
               nOptions: List[Boolean],
               iOptions: List[Boolean],
               inputFile: List[String],
             ){}

class EmptySedOptions() extends SedOptions(
  fOptions = List[String](),
  eOptions = List[String](),
  nOptions = List[Boolean](),
  iOptions = List[Boolean](),
  inputFile = List[String](),
) {}