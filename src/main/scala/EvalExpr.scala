import parser.Toolbox.Parser
import parser.Toolbox.Implicits._

object EvalExpr extends App {

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

  val pCharA = pChar('a')
//  pCharA <|> pCharA
//  pCharA !>>! pCharA
  println(pCharA("abcdefgh"))
  println(pCharA("bcdefgh"))
  println(pCharA(""))
  println(pCharA("a"))
  val listOfParser = "abcdefghijklmnopqrstuvwxyz".map(pChar)
  val pLowerAlpha: Parser[Char] = listOfParser.reduce((a, b) => a <|> b)
  println(pLowerAlpha("_______"))
  println(pLowerAlpha("xZZZ"))
}