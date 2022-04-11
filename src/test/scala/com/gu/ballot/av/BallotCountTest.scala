package com.gu.ballot.av

import com.gu.ballot.ExampleCandidates.{A, B}
import com.gu.ballot.av.Round.Outcome.ClearWinner
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BallotCountTest extends AnyFlatSpec with Matchers {
  it should "only take 1 round when there is a clear majority" in {
    val ballotCount = BallotCount(
      Preference(A, B) -> 3,
      Preference(B) -> 2
    )

    ballotCount.rounds.head.outcome shouldBe ClearWinner(A)
    ballotCount.rounds.size shouldBe 1
  }
}
