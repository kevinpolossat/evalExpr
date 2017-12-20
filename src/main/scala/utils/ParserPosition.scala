package utils

case object ParserPosition {
  def apply(inputState: InputState): ParserPosition = {
    new ParserPosition(
      inputState.currentLine,
      inputState.position.line,
      inputState.position.column)
  }
}

case class ParserPosition(currentLine: String, line: Int, column: Int)
