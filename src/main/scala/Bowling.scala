package bowling

sealed trait Frame {
  def first: Int
  def second: Option[Int]
}
sealed trait ParsedFrame

case class NormalFrame(first: Int, second: Option[Int]) extends Frame
case class LastFrame(first: Int, second: Option[Int], third: Option[Int])
    extends Frame
    with ParsedFrame

// Parsed Frame
case class StrikeFrame() extends ParsedFrame
case class SpareFrame(first: Int, second: Int) extends ParsedFrame
case class ScoreFrame(first: Int, second: Int) extends ParsedFrame
case class UnfinishedFrame() extends ParsedFrame

object Bowling {
  def getFrameScore(
      frame: Frame,
      nextFrame: Option[Frame] = None,
      nextNextFrames: Option[Frame] = None
  ): Option[Int] = {
    val parsedFrame = parseFrame(frame)
    val nextRolls = getNextRolls(nextFrame, nextNextFrames)
    parsedFrame match {
      case UnfinishedFrame() => None
      case LastFrame(first, second, third) =>
        Some(first + second.getOrElse(0) + third.getOrElse(0))
      case ScoreFrame(first, second) => Some(first + second)
      case SpareFrame(first, second) => Some(first + second + nextRolls(0))
      case StrikeFrame()             => Some(10 + nextRolls(0) + nextRolls(1))
    }
  }

  def getGameScore(game: Array[Frame]): Int = {
    game.zipWithIndex.map {
      case (frame, index) => {
        val nextFrame = getArrayOrNone(game, index + 1)
        val nextNextFrame = getArrayOrNone(game, index + 2)
        getFrameScore(frame, nextFrame, nextNextFrame) match {
          case None        => 0
          case Some(score) => score
        }
      }
    }.sum
  }

  private def parseFrame(frame: Frame): ParsedFrame = {
    if (frame.isInstanceOf[LastFrame]) {
      return frame.asInstanceOf[LastFrame]
    }
    if (frame.first == 10) {
      return StrikeFrame()
    }
    frame.second match {
      case None => UnfinishedFrame()
      case Some(second) =>
        if (frame.first + second == 10) {
          return SpareFrame(frame.first, second)
        }
        ScoreFrame(frame.first, second)
    }
  }

  private def getNextRolls(
      nextFrame: Option[Frame],
      nextNextFrames: Option[Frame]
  ): Array[Int] = {
    def frameToRolls(frame: Frame): Array[Int] = {
      frame match {
        case NormalFrame(first, None)             => Array(first)
        case NormalFrame(first, Some(second))     => Array(first, second)
        case LastFrame(first, None, _)            => Array(first)
        case LastFrame(first, Some(second), None) => Array(first, second)
        case LastFrame(first, Some(second), Some(third)) =>
          Array(first, second, third)
      }
    }

    val firstFrameRolls = nextFrame match {
      case None    => Array()
      case Some(f) => frameToRolls(f)
    }
    val secondFrameRolls = nextNextFrames match {
      case None    => Array()
      case Some(f) => frameToRolls(f)
    }

    firstFrameRolls.concat(secondFrameRolls)
  }

  private def getArrayOrNone[T](arr: Array[T], index: Int): Option[T] = {
    try {
      Some(arr(index))
    } catch {
      case _: IndexOutOfBoundsException => None
    }
  }
}
