import org.scalatest._

class InputStateTest extends FlatSpec with Matchers {
  "InputState" should "correctly read chars" in {
    InputState("").readAll should be (List())
    InputState("a").readAll should be (List('a', '\n'))
    InputState("ab").readAll should be (List('a', 'b', '\n'))
    InputState("a\nb").readAll should be (List('a', '\n', 'b', '\n'))
  }
}
