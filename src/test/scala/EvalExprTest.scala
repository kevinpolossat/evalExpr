import org.scalatest._
import EvalExpr._
import utils.{Position, SimpleStringReader}
import utils.SimpleStringReader._

class EvalExprTest extends FlatSpec with Matchers {
  val parseExpression = evalParser

  "evalParser" should "be able to parse simple double" in {
    parseExpression("123") should be(Right(123.0, SimpleStringReader("", Position(0,3))))
    parseExpression("-123") should be(Right(-123.0, SimpleStringReader("", Position(0,4))))
    parseExpression("123.4") should be(Right(123.4, SimpleStringReader("", Position(0,5))))
    parseExpression("123") should be(Right(123.0, SimpleStringReader("", Position(0,3))))
    parseExpression("-123") should be(Right(-123.0, SimpleStringReader("", Position(0,4))))
    parseExpression("-123.") should be(Right(-123.0, SimpleStringReader(".", Position(0,4))))
    parseExpression("00.4") should be(Right(0.0, SimpleStringReader("0.4", Position(0,1))))
    parseExpression("123e4") should be(Right(1230000.0, SimpleStringReader("", Position(0,5))))
    parseExpression("123.4e5") should be(Right(12340000.0, SimpleStringReader("", Position(0,7))))
    parseExpression("123.4e-5") should be(Right(0.001234, SimpleStringReader("", Position(0,8))))
    parseExpression("123.4e") should be(Right(123.4, SimpleStringReader("e", Position(0,5))))
  }

  "evalParser" should "be able to parse simple double between parenthesis" in {
    parseExpression("(123)") should be(Right(123.0, SimpleStringReader("", Position(0,5))))
    parseExpression("((123))") should be(Right(123.0, SimpleStringReader("", Position(0,7))))
    parseExpression("((123)") should be(Left("+", "unexpected ("))
    parseExpression("()") should be(Left("+", "unexpected ("))
  }

  "evalParser" should "be able to parse identifier" in {
    parseExpression("v(123)") should be(Right(123.0, SimpleStringReader("", Position(0,6))))
    parseExpression("v((123))") should be(Right(123.0, SimpleStringReader("", Position(0,8))))
    parseExpression("v((123)") should be(Left(("+", "unexpected v")))
    parseExpression("v()") should be(Left("+", "unexpected v"))
  }

  "evalParser" should "be able to parse factor" in {
    parseExpression("-v(123)") should be(Right(-123.0, SimpleStringReader("", Position(0,7))))
    parseExpression("--v(123)") should be(Right(123.0, SimpleStringReader("",Position(0,8))))
    parseExpression("---v(123)") should be(Right(-123.0, SimpleStringReader("",Position(0,9))))
    parseExpression("+v((123))") should be(Right(123.0, SimpleStringReader("", Position(0,9))))
    parseExpression("-123") should be(Right(-123.0, SimpleStringReader("", Position(0,4))))
    parseExpression("--((123))") should be(Right(123.0, SimpleStringReader("", Position(0,9))))
    parseExpression("---(123)") should be(Right(-123.0, SimpleStringReader("", Position(0,8))))
    parseExpression("+(123)") should be(Right(123.0,SimpleStringReader("", Position(0,6))))
  }
}