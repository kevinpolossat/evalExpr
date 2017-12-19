
case object InputState {
  def apply(in: String): InputState = {
    if (in.isEmpty) {
      new InputState(Array.empty[String], Position())
    }
    else {
      new InputState(in.split("\n"), Position()) // file like with line and col
    }
  }
}

case class InputState(lines: Array[String], position: Position) {
  def nextChar: (InputState, Option[Char]) = {
    val line = position.line
    val column = position.column

    if (line >= lines.length) {
      (this, None)
    }
    else {
      val currentLine = lines(line)
      if (column >= currentLine.length) {
        (this.copy(position = position.incLine), Some('\n'))
      }
      else {
        (this.copy(position = position.incColumn), Some(currentLine(column)))
      }
    }
  }

  def readAll: List[Char] = {
    def readAllLoop(inputState: InputState, acc: List[Char]): List[Char] = inputState.nextChar match {
      case (in, Some(c)) => readAllLoop(in, c :: acc)
      case _ => acc
    }
    readAllLoop(this, List.empty[Char]).reverse
  }
}
