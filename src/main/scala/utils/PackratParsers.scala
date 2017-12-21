package utils

import scala.collection.mutable.HashMap
import Parser._

trait PackratParsers[T] {
  abstract class PackratParser[T] extends Parser[T]

  implicit def parserToPackrat(p: Parser[T]): PackratParser[T] = {
    memoization(p)
  }

  val cache: HashMap[(Parser[T], Position), Result[T]] = HashMap.empty

  def memoization(parser: Parser[T]): Parser[T] = {
    new Parser[T] {
      def apply(in: Input) = {
        cache.get((parser, in.position)) match {
          case Some(res) => res
          case None =>
            val res = parser(in)
            cache.put((parser, in.position), res)
            res
        }
      }
    }
    ???
  }
}
