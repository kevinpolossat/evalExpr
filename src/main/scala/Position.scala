case class Position(line: Int = 0, column: Int = 0) {
  def incLine: Position = Position(line = line + 1)
  def incColumn: Position = this.copy(column = column + 1)
}
