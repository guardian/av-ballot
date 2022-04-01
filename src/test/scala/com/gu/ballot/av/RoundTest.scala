package com.gu.ballot.av

import com.gu.ballot.ExampleCandidates.*
import com.gu.ballot.VotesPerCandidate
import kantan.csv.{ReadError, rfc}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class RoundTest extends AnyFlatSpec with Matchers {

  it should "acknowledge candidates even if those candidates did not come first in any preference" in {
    Round(
      Preference(A, B)
    ).candidates shouldBe Set(A,B)

    Round(
      Preference(A, B),
      Preference(C, D)
    ).candidates shouldBe Set(A,B,C,D)
  }

  it should "count first-preference votes for all candidates" in {
    Round(
      Preference(A, C),
      Preference(B, C)
    ).firstPreferenceVotes.rankedByVotes.toSeq shouldBe Seq(1 -> Set(A,B), 0 -> Set(C))
  }

  it should "count votes at different preference stages" in {
    Round(
      Preference(A, C, B),
      Preference(B, C, A),
      Preference(B, C, A)
    ).preferenceStages shouldBe Seq(
      VotesPerCandidate(Map(A -> 1, B -> 2, C -> 0)),
      VotesPerCandidate(Map(A -> 0, B -> 0, C -> 3)),
      VotesPerCandidate(Map(A -> 2, B -> 1, C -> 0))
    )
  }

  it should "remove a last-placed candidate, even if they got more votes at the next stage of preferences" in {
    val summary = Round(
      Preference(A, C),
      Preference(B, C)
    )
    summary.lastPlacedCandidates shouldBe Set(C)
    summary.withoutLastPlacedCandidates.candidates shouldBe Set(A, B)
  }

  it should "not crash if the result is a tie" in {
    val summary = Round(Preference(A), Preference(B))
    summary.firstPreferenceVotes.rankedByVotes.values.head shouldBe Set(A, B)
    // summary.ultimateWinner
  }

}
