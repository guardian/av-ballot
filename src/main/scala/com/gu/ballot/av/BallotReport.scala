package com.gu.ballot.av

import com.gu.ballot.*
import com.gu.ballot.av.Round.Conclusion
import com.gu.ballot.av.Round.Outcome.{ClearWinner, Elimination, EssentialTie}

case class BallotReport(electorateSize: Int, ballotCount: BallotCount) {

  val turnout: Turnout = Turnout(ballotCount.numVotes, electorateSize)

  val report: String = {
    (ballotCount.conclusion match {
      case ClearWinner(winner) =>
        s"The winner of this election was candidate $winner"
      case EssentialTie(_) =>
        "The results led to an essential tie"
    }) +
      s""" with ${ballotCount.rounds.size} rounds taken to reach the result.
         |${turnout.summary}
         |
         |${ballotCount.roundsReport}
         |""".stripMargin
  }

}

object BallotReport {
  def derivingElectorateFrom(ballotCount: BallotCount) = BallotReport(ballotCount.numVotes, ballotCount)
}
