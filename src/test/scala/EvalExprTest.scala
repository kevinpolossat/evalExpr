import org.scalatest._

import EvalExpr._
import utils.SimpleStringReader._

class EvalExprTest extends FlatSpec with Matchers {
  val parseExpression = evalParser()

  "optSign" should "correctly parse plus or minus sign" in {
    val parseOptSign = pOptSign()
    parseOptSign("-") should be(Right(Some('-'), ""))
    parseOptSign("+") should be(Right(None, "+"))
    parseOptSign("ezaeza") should be(Right(None, "ezaeza"))
    parseOptSign("") should be(Right(None, ""))
  }

  "pInt" should "correctly compute parse an int" in {
    val parseInt = pInt()
    parseInt("0") should be(Right("0", ""))
    parseInt("123") should be(Right("123", ""))
    parseInt("123a") should be(Right("123", "a"))
    parseInt("") should be(Left("0 orElse 1-9 andThen many of digit", "No more input"))
    parseInt("a") should be(Left("0 orElse 1-9 andThen many of digit", "unexpected a"))
    parseInt("012") should be(Right("0", "12"))
  }

  "pOptFractionPart" should "correctly parse fractionnal part" in {
    val parseFract = pOptFractionPart()
    parseFract(".032134321") should be(Right(Some("032134321"), ""))
    parseFract("032134321") should be(Right(None, "032134321"))
    parseFract("") should be(Right(None, ""))
  }

  "pOptExponent" should "correctly parse exponent" in {
    val parseOptExponnent = pOptExponent()
    parseOptExponnent("e-321321") should be(Right(Some("-321321"), ""))
    parseOptExponnent("e-321321eza") should be(Right(Some("-321321"), "eza"))
    parseOptExponnent("e321321eza") should be(Right(Some("321321"), "eza"))
    parseOptExponnent("e+321321eza") should be(Right(Some("+321321"), "eza"))
  }

  "evalParser" should "be able to parse simple double" in {
    parseExpression("123") should be(Right(123.0, ""))
    parseExpression("-123") should be(Right(-123.0, ""))
    parseExpression("123.4") should be(Right(123.4, ""))
    parseExpression("123") should be(Right(123.0, ""))
    parseExpression("-123") should be(Right(-123.0, ""))
    parseExpression("-123.") should be(Right(-123.0, "."))
    parseExpression("00.4") should be(Right(0.0, "0.4"))
    parseExpression("123e4") should be(Right(1230000.0, ""))
    parseExpression("123.4e5") should be(Right(12340000.0, ""))
    parseExpression("123.4e-5") should be(Right(0.001234, ""))
    parseExpression("123.4e") should be(Right(123.4, "e"))
  }
  "evalParser" should "be able to parse simple double between parenthesis" in {
    parseExpression("(123)") should be(Right(123.0, ""))
    parseExpression("((123))") should be(Right(123.0, ""))
    parseExpression("((123)") should be(Left("", ""))
    parseExpression("()") should be(Left("", ""))
  }
}