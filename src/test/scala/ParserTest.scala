import org.scalatest._
import parser.Toolbox._
import parser.Toolbox.Implicits._

class ParserTest extends FlatSpec with Matchers {
  val pCharA: Parser[Char] = pChar('a')
  val pCharB: Parser[Char] = pChar('b')
  val AandA: Parser[(Char, Char)] = pCharA !>>! pCharA
  val AorB: Parser[Char] = pCharA <|> pCharB
  val aMapToA: Parser[Char] = pCharA |>> (_.toUpper)

  "Parser" should "without combination should work properly" in {
    pCharA("a") should be (Right('a', ""))
    pCharA("b") should be (Left("Expecting a got b"))
    pCharA("") should be (Left("No more input"))
    pCharA("abb") should be (Right('a', "bb"))
  }

  "Parser" should "correctly compute andElse method" in {
    AandA("aaB") should be (Right(('a', 'a'), "B"))
    AandA("abB") should be (Left("Expecting a got b"))
  }

  "Parser" should "correctly compute orElse method" in {
    AorB("bbb") should be (Right('b', "bb"))
    AorB("zzB") should be (Left("Expecting b got z"))
  }

  "Parser" should "correctly compute map method" in {
    aMapToA("aAA") should be (Right('A', "AA"))
    aMapToA("zAA") should be (Left("Expecting a got z"))
  }

  "Parser" should "correctly choice a character" in {
    val pLowerAlpha = anyOf('a' to 'z')
    pLowerAlpha("xblah") should be (Right('x', "blah"))
    pLowerAlpha("Ablah") should be (Left("Expecting z got A"))
  }

  "Parser" should "correctly sequence" in {
    val pHelloWorld = pString("Hello World !")
    pHelloWorld("Hello World !Ici, c'est Paris") should be (Right("Hello World !", "Ici, c'est Paris"))
    pHelloWorld("Hello Worxd !Ici, c'est Paris") should be (Left("Expecting l got x"))
  }
}
