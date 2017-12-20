package utils

trait IReader[T] {
  def head: T
  def rest: IReader[T]
  def position: Position
  def atEnd: Boolean
}
