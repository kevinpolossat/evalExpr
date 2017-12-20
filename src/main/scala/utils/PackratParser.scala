package utils

import scala.collection.immutable.HashMap
import Parser._

abstract class PackratParser[T] extends Parser[T] {

  implicit def parserToPackrat(p: Parser[T]): PackratParser[T] = {
    memoization(p)
  }

  val cache: HashMap[(Parser[_], Position), Result[T]] = HashMap.empty
  def memoization(parser: Parser[T]): PackratParser[T] = {
    ???
  }
}
