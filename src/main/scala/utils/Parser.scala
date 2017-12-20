package utils

import utils.Parser.{Input, ParserFunc, ParserLabel, Result}

case object Parser {
  type ErrorMessage = String
  type Input = String // utils.InputState
  type RemainingInput = String // utils.InputState
  type ParserLabel = String
  type Success[T] = (T, RemainingInput)
  type Failure = (ParserLabel, ErrorMessage/*, utils.ParserPosition*/)
  type Result[T] = Either[Failure, Success[T]]
  type ParserFunc[T] = Input => Result[T]

  def returnP[T](x: T): Parser[T] = Parser((s: Input) => Right(x, s))

  def applyP[I, O](fP: Parser[I => O], xP: Parser[I]): Parser[O] = fP !>>! xP |>> { case (f, x) => f(x) }

  def choice[T](xs: TraversableOnce[Parser[T]]): Parser[T] = xs.reduce((a, b) => a <|> b)

  def sequence[T](xs: TraversableOnce[Parser[T]]): Parser[List[T]] = {
    xs.map(_ |>> (List(_))).reduce { (a, b) => (a !>>! b) |>> { case (lhs, rhs) => lhs ::: rhs } }
  }

  def parserZeroOrMore[T](parser: Parser[T], input: Input): (List[T], RemainingInput) = {
    parser(input) match {
      case Left(_) => (List.empty[T], input)
      case Right((firstValue, remainingInput)) =>
        val (values, retRemainingInput) = parserZeroOrMore(parser, remainingInput)
        (firstValue :: values, retRemainingInput)
    }
  }

  def many[T](p: Parser[T]): Parser[List[T]] = {
    Parser({ input: Input => Right(parserZeroOrMore(p, input)) }, s"many of ${p.label}")
  }

  def opt[T](p: Parser[T]): Parser[Option[T]] = {
    val some: Parser[Option[T]] = p |>> Some.apply
    val none: Parser[Option[T]] = returnP(None)
    some <|> none
  }

  def many1[T](parser: Parser[T]): Parser[List[T]] = {
    Parser({ input: String  => {
      parser(input).map { case (firstChar, remainingInput) =>
        val (values, remainingInputRet) = parserZeroOrMore(parser, remainingInput)
        (firstChar :: values, remainingInputRet)
      }
    }
    }, s"one or many of ${parser.label}")
  }

  def satisfy(predicate: Char => Boolean, label: String): Parser[Char] = {
    Parser({ in: String => {
      val cOpt = in.headOption
      cOpt match {
        case Some(c) => if (predicate(c)) Right(c, in.tail) else Left(label, s"unexpected $c"/*, utils.ParserPosition(in)*/)
        case _ => Left(label, "No more input"/*, utils.ParserPosition(in)*/)
      }
    }
    }, label)
  }

  def anyOf(chars: TraversableOnce[Char]): Parser[Char] = {
    val newLabel = s"any of ${chars.mkString("[", ", ", "]")}"
    val pChars = chars.map { charToMatch => satisfy({ c => c == charToMatch }, charToMatch.toString) }
    choice(pChars) <|?|> newLabel
  }

  def between[A, B, C](p1: Parser[A], p2: Parser[B], p3: Parser[C]): Parser[B] = p1 >>! p2 !>> p3

  def apply[T](func: ParserFunc[T], label: ParserLabel = "unknown"): Parser[T] = new Parser[T](label){ def apply(in: String) = func(in) }

  def prettyString[T](res: Result[T]): String = res match {
    case Right(a) => s"$a"
    case Left((label, errorMessage/*, pos*/)) =>
/*      val errorLine = pos.currentLine
      val colPos = pos.column
      val linePos = pos.line
      val strError = "%*s^%s".format(colPos, "", errorMessage)
      s"Line: $linePos, col: $colPos Error parsing $label\n$errorLine\n$strError"*/
      s"Error parsing expecting $label\n$errorMessage"
  }
}

abstract case class Parser[T](label: ParserLabel = "unknown") extends ParserFunc[T] {

  def apply(in: String): Result[T]

  def updateLabel(newLabel: ParserLabel): Parser[T] = {
    Parser({
      input => {
        this(input) match {
          case Right(s) => Right(s)
          case Left((_, errorMessage/*, pos*/)) => Left(newLabel, errorMessage/*, pos*/)
        }
      }
    }, newLabel)
  }

  def andThen[B](other: Parser[B]): Parser[(T, B)] = {
    val andThenLabel = s"$label andThen ${other.label}"
    Parser({ s: Input => {
      this(s).flatMap { case (matched1, remaining1) =>
        other(remaining1).map { case (matched2, remaining2) => ((matched1, matched2), remaining2) }
      }
    }
    }) <|?|> andThenLabel
  }

  def orElse(other: Parser[T]): Parser[T] = {
    val orElseLabel = s"$label orElse ${other.label}"
    Parser({ s: Input => {
      val p1 = this(s)
      if (p1.isLeft) other(s) else p1
    }
    }) <|?|> orElseLabel
  }

  def map[B](f: T => B): Parser[B] = Parser(this(_).map(x => (f(x._1), x._2)), label)

  def <|>(other: Parser[T]): Parser[T] = orElse(other)

  def |>>[B](f: T => B): Parser[B] = map(f)

  def !>>![B](other: Parser[B]): Parser[(T, B)] = andThen(other)

  def !>>[B](other: Parser[B]): Parser[T] = andThen(other).map { case (a, _) => a }

  def >>![B](other: Parser[B]): Parser[B] = andThen(other).map { case (_, b) => b }

  def <|?|>(newLabel: ParserLabel): Parser[T] = updateLabel(newLabel)
}
