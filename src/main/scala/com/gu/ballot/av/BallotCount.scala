package com.gu.ballot.av

import com.gu.ballot.*
import com.gu.ballot.VotesPerCandidate
import com.gu.ballot.av.Round.Conclusion
import com.gu.ballot.av.Round.Outcome.{ClearWinner, Elimination, EssentialTie}

case class BallotCount(countByPreference: Map[Preference, Int]) {

  val firstRound: Round = Round(countByPreference)
  val firstPreferenceVotes: VotesPerCandidate = firstRound.firstPreferenceVotes
  val numVotes: Int = firstPreferenceVotes.numVotes
  val conclusion: Conclusion = firstRound.ultimateConclusion

  val rounds: LazyList[Round] = LazyList.unfold(Option(firstRound)) { _.map { round =>
      round -> round.nextRound
    }
  }

  val roundsReport: String =
    rounds.zipWithIndex.map { case (round, index) => s"In round ${index+1}, ${round.summary}"}.mkString("\n\n")

}

object BallotCount {
  def apply(ballotPatterns: Preference*): BallotCount =
    BallotCount(ballotPatterns.frequencyCount)

  def apply(firstElement: (Preference, Int), otherElements: (Preference, Int)*): BallotCount =
    BallotCount((firstElement +: otherElements).toMap)
}