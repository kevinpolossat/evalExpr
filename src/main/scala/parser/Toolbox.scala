package parser

object Toolbox {
  type ErrorMessage = String
  type RemainingInput = String
  type Parser[T] = String => Either[ErrorMessage, (T, RemainingInput)]

  def andThen[A, B](parser1: Parser[A], parser2: Parser[B]): Parser[(A, B)] = {
    s: String => {
      parser1(s).flatMap { case (matched1, remaining1) =>
        parser2(remaining1).map { case (matched2, remaining2) => ((matched1, matched2), remaining2) }
      }
    }
  }

  def orElse[T](parser1: Parser[T], parser2: Parser[T]): Parser[T] = {
    s: String => {
      val p1 = parser1(s)
      if (p1.isLeft) parser2(s) else p1
    }
  }

  def map[A, B](parser1: Parser[A], f: A => B): Parser[B] = parser1(_).map(x => (f(x._1), x._2))

  object Implicits {
    implicit class ParserImpl[A](parser: Parser[A]) {
      def andThen[B](other: Parser[B]): Parser[(A, B)] = Toolbox.andThen(parser, other)
      def orElse(other: Parser[A]): Parser[A] = Toolbox.orElse(parser, other)
      def map[B](f: A => B): Parser[B] = Toolbox.map(parser, f)
      def <|>(other: Parser[A]): Parser[A] = orElse(other)
      def |>>[B](f: A => B): Parser[B] = map(f)
      def !>>![B](other: Parser[B]): Parser[(A, B)] = andThen(other)
    }
  }

  import Implicits._

  def choice[T](xs: TraversableOnce[Parser[T]]): Parser[T] = xs.reduce((a, b) => a <|> b)
  def sequence[T](xs: TraversableOnce[Parser[T]]): Parser[List[T]] = {
    xs.map(_ |>> (List(_))).reduce{ (a, b) => (a !>>! b) |>> { case (lhs, rhs) => lhs ::: rhs } }
  }
  def many() = ???
  def many1() = ???
  def opt() = ???

  def pChar(charToMatch: Char): Parser[Char] = {
    s: String => {
      if (s.isEmpty) {
        Left("No more input")
      }
      else {
        val c = s.head
        if (c == charToMatch) Right(c, s.tail) else Left(s"Expecting $charToMatch got $c")
      }
    }
  }
  def pString(strToMatch: String): Parser[String] = {
    val x = sequence(strToMatch.map(pChar))
    x |>> (_.mkString)
  }
  def anyOf(chars: TraversableOnce[Char]): Parser[Char] = choice(chars.map(pChar))

}
