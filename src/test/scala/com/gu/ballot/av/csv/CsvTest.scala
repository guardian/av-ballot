package com.gu.ballot.av.csv

import kantan.csv.rfc
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import kantan.csv.*
import kantan.csv.ops.*
import cats.implicits.*
import com.gu.ballot.av.BallotReport
import com.gu.ballot.av.csv.CsvSrc.UrlSrc

class CsvTest extends AnyFlatSpec with Matchers {
  it should "be a temporary test that prints stuff " in {
    val rawData: java.net.URL = getClass.getResource("/dummy-election.responses.csv")

    println("\n"+BallotReport.derivingElectorateFrom(VotesCsvParser.parse(UrlSrc(rawData))).report)
  }
}
