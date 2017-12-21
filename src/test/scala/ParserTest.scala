import org.scalatest._
import utils.Parser
import utils.Parser._
import utils._
import utils.SimpleStringReader
import utils.SimpleStringReader._

class ParserTest extends FlatSpec with Matchers {
/*  val pCharA: Parser[Char] = satisfy(c => c == 'a', "a")
  val pCharB: Parser[Char] = satisfy(c => c == 'b', "b")
  val AandA: Parser[(Char, Char)] = pCharA !>>! pCharA
  val AorB: Parser[Char] = pCharA <|> pCharB
  val aMapToA: Parser[Char] = pCharA |>> (_.toUpper)

  "utils.Parser" should "without combination should work properly" in {
    pCharA("a") should be (Right('a', SimpleStringReader("",Position(0,1))))
    pCharA("b") should be (Left("a", "unexpected b"))
    pCharA("") should be (Left("a", "No more input"))
    pCharA("abb") should be (Right('a', SimpleStringReader("bb", Position(0,1))))
  }

  "utils.Parser" should "correctly compute andElse method" in {
    AandA("aaB") should be (Right(('a', 'a'), SimpleStringReader("B", Position(0,2))))
    AandA("abB") should be (Left("a andThen a", "unexpected b"))
  }

  "utils.Parser" should "correctly compute orElse method" in {
    AorB("bbb") should be (Right('b', SimpleStringReader("bb", Position(0,1))))
    AorB("zzB") should be (Left("a orElse b", "unexpected z"))
  }

  "utils.Parser" should "correctly compute map method" in {
    aMapToA("aAA") should be (Right('A', SimpleStringReader("AA", Position(0,1))))
    aMapToA("zAA") should be (Left("a", "unexpected z"))
  }

  "utils.Parser" should "correctly choice a character" in {
    val pLowerAlpha = anyOf('a' to 'z')
    pLowerAlpha("xblah") should be (Right('x', SimpleStringReader("blah", Position(0,1))))
    pLowerAlpha("Ablah") should be (Left("any of [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]", "unexpected A"))
  }

  "utils.Parser" should "correctly evaluate between" in {
    val pABetweenParenthesis = Parser.between(
      Parser.satisfy(_ == '(', "parse open parenthesis"),
      pCharA,
      Parser.satisfy(_ == ')', "parse closing parenthesis"))
    pABetweenParenthesis("(a)") should be (Right('a', SimpleStringReader("", Position(0,3))))
    pABetweenParenthesis("") should be (Left("parse open parenthesis andThen a andThen parse closing parenthesis", "No more input"))
    pABetweenParenthesis("(a") should be (Left("parse open parenthesis andThen a andThen parse closing parenthesis", "No more input"))
    pABetweenParenthesis("a)") should be (Left("parse open parenthesis andThen a andThen parse closing parenthesis", "unexpected a"))
  }*/
}
