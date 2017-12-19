import Parser._

case object Parser {
  type ErrorMessage = String
  type Input = String // InputState
  type RemainingInput = String // InputState
  type ParserLabel = String
  type Success[T] = (T, RemainingInput)
  type Failure = (ParserLabel, ErrorMessage/*, ParserPosition*/)
  type Result[T] = Either[Failure, Success[T]]
  type ParserFunc[T] = Input => Result[T]

  def returnP[T](x: T): Parser[T] = new Parser((s: Input) => Right(x, s))

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
    new Parser({ input: Input => Right(parserZeroOrMore(p, input)) }, s"many of ${p.label}")
  }

  def opt[T](p: Parser[T]): Parser[Option[T]] = {
    val some: Parser[Option[T]] = p |>> Some.apply
    val none: Parser[Option[T]] = returnP(None)
    some <|> none
  }

  def many1[T](parser: Parser[T]): Parser[List[T]] = {
    new Parser({ input  => {
      parser(input).map { case (firstChar, remainingInput) =>
        val (values, remainingInputRet) = parserZeroOrMore(parser, remainingInput)
        (firstChar :: values, remainingInputRet)
      }
    }
    }, s"one or many of ${parser.label}")
  }

  def satisfy(predicate: Char => Boolean, label: String): Parser[Char] = {
    new Parser({ in => {
      val cOpt = in.headOption
      cOpt match {
        case Some(c) => if (predicate(c)) Right(c, in.tail) else Left(label, s"unexpected $c"/*, ParserPosition(in)*/)
        case _ => Left(label, "No more input"/*, ParserPosition(in)*/)
      }
    }
    }, label)
  }

  def anyOf(chars: TraversableOnce[Char]): Parser[Char] = {
    val newLabel = s"any of ${chars.mkString("[", ", ", "]")}"
    val pChars = chars.map { charToMatch => satisfy({ c => c == charToMatch }, charToMatch.toString) }
    choice(pChars) <|?|> newLabel
  }

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

case class Parser[T](parserFunc: ParserFunc[T], label: ParserLabel = "unknown") {

  def updateLabel(newLabel: ParserLabel): Parser[T] = {
    Parser({
      input => {
        parserFunc(input) match {
          case Right(s) => Right(s)
          case Left((_, errorMessage/*, pos*/)) => Left(newLabel, errorMessage/*, pos*/)
        }
      }
    }, newLabel)
  }

  def apply(input: Input): Result[T] = parserFunc(input)

  def andThen[B](other: Parser[B]): Parser[(T, B)] = {
    val andThenLabel = s"$label andThen ${other.label}"
    Parser({ s: Input => {
      parserFunc(s).flatMap { case (matched1, remaining1) =>
        other(remaining1).map { case (matched2, remaining2) => ((matched1, matched2), remaining2) }
      }
    }
    }) <|?|> andThenLabel
  }

  def orElse(other: Parser[T]): Parser[T] = {
    val orElseLabel = s"$label orElse ${other.label}"
    Parser({ s: Input => {
      val p1 = parserFunc(s)
      if (p1.isLeft) other(s) else p1
    }
    }) <|?|> orElseLabel
  }

  def map[B](f: T => B): Parser[B] = Parser(parserFunc(_).map(x => (f(x._1), x._2)), label)

  def <|>(other: Parser[T]): Parser[T] = orElse(other)

  def |>>[B](f: T => B): Parser[B] = map(f)

  def !>>![B](other: Parser[B]): Parser[(T, B)] = andThen(other)

  def !>>[B](other: Parser[B]): Parser[T] = andThen(other).map { case (a, _) => a }

  def >>![B](other: Parser[B]): Parser[B] = andThen(other).map { case (_, b) => b }

  def <|?|>(newLabel: ParserLabel): Parser[T] = updateLabel(newLabel)
}
