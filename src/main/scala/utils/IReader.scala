package utils

trait IReader[T] {
  def head: T
  def headOption: Option[T]
  def tail: IReader[T]
  def position: Position
  def atEnd: Boolean
}
