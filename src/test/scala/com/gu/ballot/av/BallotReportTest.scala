package com.gu.ballot.av

import com.gu.ballot.ExampleCandidates.{A, B, C, D}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BallotReportTest extends AnyFlatSpec with Matchers {
  it should "be cool" in {
    println(BallotReport(16, BallotCount(
      Preference(A, B) -> 3,
      Preference(B) -> 2
    )).report)
  }


}
