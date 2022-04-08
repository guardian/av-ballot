package com.gu.ballot

import com.gu.ballot.ExampleCandidates.{A, B, C, D}
import com.gu.ballot.av.{Preference, Round}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ExampleCandidates.*
import org.scalatest.OptionValues

class VotesPerCandidateTest extends AnyFlatSpec with Matchers with OptionValues {

  it should "acknowledge candidates even if those candidates did not come first in any preference" in {
    val twoVotes = VotesPerCandidate(Map(A -> 2))
    twoVotes.majorityThreshold shouldBe 2
    twoVotes.wouldHaveMajorityWith(1) shouldBe false
    twoVotes.wouldHaveMajorityWith(2) shouldBe true

    val threeVotes = VotesPerCandidate(Map(A -> 3))
    threeVotes.majorityThreshold shouldBe 2
    threeVotes.wouldHaveMajorityWith(1) shouldBe false
    threeVotes.wouldHaveMajorityWith(2) shouldBe true

    val fourVotes = VotesPerCandidate(Map(A -> 4))
    fourVotes.majorityThreshold shouldBe 3
    fourVotes.wouldHaveMajorityWith(2) shouldBe false
    fourVotes.wouldHaveMajorityWith(3) shouldBe true
  }

  it should "calculate the set of lowest-ranked candidates that collectively have fewer votes than any other candidate" in {
    /* From https://en.wikipedia.org/wiki/Instant-runoff_voting#Process :
     * "If there is an exact tie for last place in numbers of votes, various tie-breaking rules
     *  determine which candidate to eliminate. The set of all candidates with the fewest first-order
     *  votes whose votes together total less than any other candidate's can be eliminated without
     *  changing the outcome. This bulk elimination can bypass irrelevant ties, for example if one
     *  candidate receives 15 first-order votes and four others receive 5, 5, 3, and 1, and no other
     *  candidate receives fewer than 15, all four of the latter candidates will be eliminated during
     *  the next four rounds, and so can be eliminated immediately without considering the tie."
     */
    val acc: Acc = VotesPerCandidate(
      Map(A -> 16, B -> 15, C -> 5, D -> 5, E -> 3, F -> 1)
    ).candidatesThatCollectivelyHaveFewerVotesThanAnyOtherCandidate.value
    acc.candidates shouldBe Set(C, D, E, F)
    acc.collectiveTotalVote shouldBe 14
  }
}