import Parser._

object EvalExpr extends App {
  /**
    * expression
    * : term
    * | expression '+' term
    * | expression '-' term
    * ;
    */
  def pExpression(): Parser[Double] = {
    // ==> choice(parseTerm | parseExpression andThen choice(parsePlus, parseMinus)andThen parseTerm)
    Parser.choice(Seq(pTerm(), pAddition(), pDifference()))
  }

  def pAddition(): Parser[Double] = ???

  def pDifference(): Parser[Double] = ???

  def pMultiplication(): Parser[Double] = ???

  def pDivision(): Parser[Double] = ???

  def pModulus(): Parser[Double] = ???

  /**
    * term
    * : factor
    * | term '*' factor
    * | term '/' factor
    * | term '%' factor
    * ;
    */
  def pTerm(): Parser[Double] = {
    // ==> choice(parseFactor() | parseTerm choice(parseMult, parseDivide, parseModulus) andThen parseFactor)
    Parser.choice(
      Seq(
        pFactor(),
        pMultiplication(),
        pDivision(),
        pModulus()))
    ???
  }

  def pMinus(): Parser[Char] = pChar('-')
  def pPlus(): Parser[Char] = pChar('+')
  def pMinusOrPlus(): Parser[Char] = pMinus() <|> pPlus()
  /**
    * factor
    * : primary
    * | '-' factor
    * | '+' factor
    * ;
    */
  def pFactor(): Parser[Double] = {
    Parser
      .many(pMinusOrPlus())
      .andThen(pPrimary())
      .map{ case (signs, value) => (signs.mkString + value.toString).toDouble }
  }

  /**
    * primary
    * : IDENTIFIER
    * | INTEGER
    * | FLOATING_POINT_LITERAL
    * | '(' expression ')'
    * ;
    */
  def pPrimary(): Parser[Double] = {
    // ==> choice(parseParenthesisOpen andThen parseExpression andThen parseParenthesisClose | parseFloat)
    pDouble()
  }

  /**
    * IDENTIFIER
    * : "v(" expression ")"
    * @return
    */
  def pIdentifier(): Parser[Double] = { // TODO Define list of identifier
    // ==> "v(" between ")"
    ???
  }

  /**
    * '(' expr ')'
    */
  def pBetweenParenthesis(): Parser[Double] = ???

  def pString(s: String): Parser[String] = (sequence(s.map(pChar)) |>> (_.mkString)) <|?|> s

  def pChar(charToMatch: Char): Parser[Char] = satisfy(c => charToMatch == c, charToMatch.toString)

  def pDigit(): Parser[Char] = satisfy(c => c.isDigit, "digit")

  def pNonZeroDigit(): Parser[Char] = {
    satisfy(c => c.isDigit && c != '0', "1-9")
  }

  def pZero(): Parser[String] = pString("0")

  def pNonZero(): Parser[String] = {
    pNonZeroDigit() !>>! many(pDigit()) |>> { case (first, rest) => first.toString + rest.mkString }
  }

  def pOptPlusMinus(): Parser[Option[Char]] = opt(pChar('-') <|> pChar('+'))

  def pExponentChar(): Parser[Char] = pChar('e') <|> pChar('E')

  def pOptExponent(): Parser[Option[String]] = {
    opt(
      pExponentChar >>! pOptPlusMinus() !>>! many1(pDigit()) |>> { case (optSign, digits) =>
        optSign.getOrElse("") + digits.mkString
      })
  }

  def pPoint(): Parser[Char] = pChar('.')

  def pOptFractionPart(): Parser[Option[String]] = opt(pPoint() >>! (many1(pDigit()) |>> (_.mkString)))

  def pInt(): Parser[String] = pZero() <|> pNonZero()

  def pOptSign(): Parser[Option[Char]] = opt(pChar('-'))

  def convertToDouble(elements: (((Option[Char], String), Option[String]), Option[String])): Double = elements match {
    case (((optSign, intPart), optFractPart), optExponent) =>
      val signEval = optSign.getOrElse("")
      val fractPartEval = optFractPart.map { f => "." + f }.getOrElse("")
      val exponentEval = optExponent.map { ePart => "e" + ePart }.getOrElse("")
      (signEval + intPart + fractPartEval + exponentEval).toDouble
  }

  def pDouble(): Parser[Double] = pOptSign() !>>! pInt() !>>! pOptFractionPart() !>>! pOptExponent() |>> convertToDouble

  def pParenthesisOpen() = pChar('(')

  def pParenthesisClose() = pChar(')')

  def evalParser() = pExpression()


  /**
    * expr : addition
    */
  def eval(in: String): Double = ???
}