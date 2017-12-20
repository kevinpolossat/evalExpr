import org.scalatest._
import utils.InputState

class InputStateTest extends FlatSpec with Matchers {
  "utils.InputState" should "correctly read chars" in {
    InputState("").readAll should be (List())
    InputState("a").readAll should be (List('a', '\n'))
    InputState("ab").readAll should be (List('a', 'b', '\n'))
    InputState("a\nb").readAll should be (List('a', '\n', 'b', '\n'))
  }
}
