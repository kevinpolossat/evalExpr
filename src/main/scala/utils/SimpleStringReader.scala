package utils

case object SimpleStringReader {
  def apply(str: String): SimpleStringReader = new SimpleStringReader(str, Position())
  implicit def stringToStringReader(str: String): IReader[Char] = SimpleStringReader(str)
}

case class SimpleStringReader(in: String, position: Position = Position()) extends IReader[Char] {
  override def head: Char = in.head

  override def tail: IReader[Char] = SimpleStringReader(in.tail, position.incColumn)

  override def atEnd: Boolean = in.isEmpty

  override def headOption: Option[Char] = in.headOption
}
