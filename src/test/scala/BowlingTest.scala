import bowling._

class BowlingTest extends org.scalatest.funsuite.AnyFunSuite {
  test("Score Frame") {
    val frame = Frame(3, Some(4))
    assertResult(Some(7))(Bowling.getScore(frame))
  }

  test("Spare Frame") {
    val frame = Frame(3, Some(7))
    val nextFrame = Frame(5, Some(2))
    assertResult(Some(15))(Bowling.getScore(frame, Some(nextFrame)))
  }

  test("Strike Frame") {
    val frame = Frame(10, None)
    val nextFrame = Frame(5, Some(2))
    val nextNextFrame = Frame(5, Some(2))

    assertResult(Some(17))(
      Bowling.getScore(frame, Some(nextFrame), Some(nextNextFrame))
    )
  }

  test("Consequtive Strike Frame") {
    val frame = Frame(10, None)
    val nextFrame = Frame(10, None)
    val nextNextFrame = Frame(5, Some(2))

    assertResult(Some(25))(
      Bowling.getScore(frame, Some(nextFrame), Some(nextNextFrame))
    )
  }
}
