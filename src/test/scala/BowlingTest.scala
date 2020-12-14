import bowling._

class BowlingScoreTest extends org.scalatest.funsuite.AnyFunSuite {
  test("Score Frame") {
    val frame = NormalFrame(3, Some(4))
    assertResult(Some(7))(Bowling.getFrameScore(frame))
  }

  test("Spare Frame") {
    val frame = NormalFrame(3, Some(7))
    val nextFrame = NormalFrame(5, Some(2))
    assertResult(Some(15))(Bowling.getFrameScore(frame, Some(nextFrame)))
  }

  test("Spare frame without consequtive frame") {
    val frame = NormalFrame(3, Some(7))
    assertResult(None)(Bowling.getFrameScore(frame, None))
  }

  test("Strike Frame") {
    val frame = NormalFrame(10, None)
    val nextFrame = NormalFrame(5, Some(2))
    val nextNextFrame = NormalFrame(5, Some(2))

    assertResult(Some(17))(
      Bowling.getFrameScore(frame, Some(nextFrame), Some(nextNextFrame))
    )
  }

  test("Strike Frame without consequtive frames") {
    val frame = NormalFrame(10, None)

    assertResult(None)(
      Bowling.getFrameScore(frame, None, None)
    )
  }

  test("Strike Frame with strike frame but no consequtive frame") {
    val frame = NormalFrame(10, None)

    assertResult(None)(
      Bowling.getFrameScore(frame, Some(frame), None)
    )
  }

  test("Consequtive Strike Frame") {
    val frame = NormalFrame(10, None)
    val nextFrame = NormalFrame(10, None)
    val nextNextFrame = NormalFrame(5, Some(2))

    assertResult(Some(25))(
      Bowling.getFrameScore(frame, Some(nextFrame), Some(nextNextFrame))
    )
  }

  test("Strike before last frame") {
    val frame = NormalFrame(10, None)
    val nextFrame = LastFrame(10, Some(10), Some(8))

    assertResult(Some(30))(
      Bowling.getFrameScore(frame, Some(nextFrame), None)
    )
  }

  test("Last frame with score") {
    val frame = LastFrame(5, Some(3), None)
    assertResult(Some(8))(
      Bowling.getFrameScore(frame, None, None)
    )
  }

  test("Last frame with spare") {
    val frame = LastFrame(5, Some(5), Some(7))
    assertResult(Some(17))(
      Bowling.getFrameScore(frame, None, None)
    )
  }

  test("Last frame with strike") {
    val frame = LastFrame(10, Some(10), Some(7))
    assertResult(Some(27))(
      Bowling.getFrameScore(frame, None, None)
    )
  }
}

class BowlingGameTest extends org.scalatest.funsuite.AnyFunSuite {
  test("All strike game") {
    val game: Array[Frame] =
      (Array.fill(9) { NormalFrame(10, None) }) :+ LastFrame(
        10,
        Some(10),
        Some(10)
      )
    assertResult(300)(Bowling.getGameScore(game))
  }

  test("All spare game") {
    val game: Array[Frame] =
      (Array.fill(9) { NormalFrame(5, Some(5)) }) :+ LastFrame(
        5,
        Some(5),
        Some(5)
      )
    assertResult(150)(Bowling.getGameScore(game))
  }

  test("All score game") {
    val game: Array[Frame] =
      (Array.fill(9) { NormalFrame(9, Some(0)) }) :+ LastFrame(
        9,
        Some(0),
        None
      )
    assertResult(90)(Bowling.getGameScore(game))
  }
}
