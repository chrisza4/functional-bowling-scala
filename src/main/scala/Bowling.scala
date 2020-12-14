package bowling

case class Frame(first: Int, second: Option[Int])
sealed trait ParsedFrame
case class StrikeFrame() extends ParsedFrame
case class SpareFrame(first: Int, second: Int) extends ParsedFrame
case class ScoreFrame(first: Int, second: Int) extends ParsedFrame
case class UnfinishedFrame() extends ParsedFrame

object Bowling {
  def getScore(
      frame: Frame,
      nextFrame: Option[Frame] = None,
      nextNextFrames: Option[Frame] = None
  ): Option[Int] = {
    def parseFrame(frame: Frame): ParsedFrame = {
      if (frame.first == 10) {
        return StrikeFrame()
      }
      frame.second match {
        case None => UnfinishedFrame()
        case Some(second) =>
          if (frame.first + second == 10) SpareFrame(frame.first, second)
          else ScoreFrame(frame.first, second)
      }
    }

    def getNextRolls(): Array[Int] = {
      val firstFrame = nextFrame match {
        case None                             => Array()
        case Some(Frame(first, None))         => Array(first)
        case Some(Frame(first, Some(second))) => Array(first, second)
      }
      val secondFrame = nextNextFrames match {
        case None                             => Array()
        case Some(Frame(first, None))         => Array(first)
        case Some(Frame(first, Some(second))) => Array(first, second)
      }

      firstFrame.concat(secondFrame)
    }

    val parsedFrame = parseFrame(frame)

    parsedFrame match {
      case UnfinishedFrame()         => None
      case ScoreFrame(first, second) => Some(first + second)
      case SpareFrame(first, second) => {
        return Some(first + second + getNextRolls()(0))
      }
      case StrikeFrame() => {
        val nextRoll = nextFrame match {
          case None        => return None
          case Some(frame) => frame.first
        }
        val nextNextRoll = (nextFrame, nextNextFrames) match {
          case (None, _)                             => return None
          case (Some(Frame(first, Some(second))), _) => second
          case (Some(Frame(_, None)), None)          => return None
          case (Some(Frame(_, None)), Some(Frame(nextNextFrameFirst, _))) =>
            nextNextFrameFirst
        }
        return Some(10 + getNextRolls()(0) + getNextRolls()(1))
      }
    }
  }
}
