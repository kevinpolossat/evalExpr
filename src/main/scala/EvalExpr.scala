import utils.Parser._
import utils.Parser


case object EvalExpr {

  /**
    * expression
    * : term
    * | expression '+' term
    * | expression '-' term
    * ;
    */
  private def pExpression: Parser[Double] = {
    pTerm <|> (pExpression !>>! pMinusOrPlus !>>! pTerm |>> { case ((lhs, sign), rhs) =>
        if (sign.asInstanceOf[Char] == '-') lhs - rhs
        else lhs + rhs
    })
  }

  /**
    * term
    * : factor
    * | term '*' factor
    * | term '/' factor
    * | term '%' factor
    * ;
    */
  private def pTerm: Parser[Double] = {
    // ==> choice(parseFactor | parseTerm choice(parseMult, parseDivide, parseModulus) andThen parseFactor)
    pFactor
  }

  /**
    * factor
    * : primary
    * | '-' factor
    * | '+' factor
    * ;
    */
  private def pFactor: Parser[Double] = {
    pPrimary <|> (
      (pMinusOrPlus !>>! pFactor) |>> { case (sign, primary) =>
        if (sign.asInstanceOf[Char] == '-') -primary else primary })
  }

  /**
    * primary
    * : IDENTIFIER
    * | INTEGER
    * | FLOATING_POINT_LITERAL
    * | '(' expression ')'
    * ;
    */
  private def pPrimary: Parser[Double] = {
    pDouble <|> pIdentifier <|> expressionBetweenParenthesis
  }

  private def pMinus = pChar('-')
  private def pPlus = pChar('+')
  private def pMinusOrPlus = pMinus <|> pPlus

  /**
    * identifier
    * : "v(" expression ')'
    * ;
    */
  private def pIdentifier: Parser[Double] = { // TODO add sqrt
    pChar('v') >>! pParenthesisOpen >>! pExpression !>> pParenthesisClose
  }

  private def expressionBetweenParenthesis: Parser[Double] = pParenthesisOpen >>! pExpression !>> pParenthesisClose
  
  private def pChar(charToMatch: Char) = satisfy(c => charToMatch == c, charToMatch.toString)

  private def pString(s: String) = (sequence(s.map(pChar)) |>> (_.mkString)) <|?|> s

  private def pDigit = satisfy(c => c.isDigit, "digit")

  private def pNonZeroDigit = {
    satisfy(c => c.isDigit && c != '0', "1-9")
  }

  private def pZero = pString("0")

  private def pNonZero = {
    pNonZeroDigit !>>! many(pDigit) |>> { case (first, rest) => first.toString + rest.mkString }
  }

  private def pOptPlusMinus = opt(pChar('-') <|> pChar('+'))

  private def pExponentChar = pChar('e') <|> pChar('E')

  private def pOptExponent = {
    opt(
      pExponentChar >>! pOptPlusMinus !>>! many1(pDigit) |>> { case (optSign, digits) =>
        optSign.getOrElse("") + digits.mkString
      })
  }

  private def pPoint = pChar('.')

  private def pOptFractionPart = opt(pPoint >>! (many1(pDigit) |>> (_.mkString)))

  private def pInt = pZero <|> pNonZero

  private def pOptSign = opt(pChar('-'))

  private def convertToDouble(elements: (((Option[Char], String), Option[String]), Option[String])): Double = elements match {
    case (((optSign, intPart), optFractPart), optExponent) =>
      val signEval = optSign.getOrElse("")
      val fractPartEval = optFractPart.map { f => "." + f }.getOrElse("")
      val exponentEval = optExponent.map { ePart => "e" + ePart }.getOrElse("")
      (signEval + intPart + fractPartEval + exponentEval).toDouble
  }

  private def pDouble = pOptSign !>>! pInt !>>! pOptFractionPart !>>! pOptExponent |>> convertToDouble

  private def pParenthesisOpen = pChar('(')

  private def pParenthesisClose = pChar(')')

  def evalParser = pExpression


  /**
    * expr : addition
    */
  def eval(in: String): Double = ???
}
