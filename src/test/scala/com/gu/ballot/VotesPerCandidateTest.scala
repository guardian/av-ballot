package com.gu.ballot

import com.gu.ballot.ExampleCandidates.{A, B, C, D}
import com.gu.ballot.av.{Preference, Round}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ExampleCandidates.*

class VotesPerCandidateTest extends AnyFlatSpec with Matchers {

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
}