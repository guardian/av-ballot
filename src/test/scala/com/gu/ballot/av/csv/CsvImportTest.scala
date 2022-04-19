package com.gu.ballot.av.csv

import com.gu.ballot.av.BallotReport
import com.gu.ballot.av.csv.CsvSrc.UrlSrc
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CsvImportTest  extends AnyFlatSpec with Matchers {
  it should "be a temporary test that prints stuff " in {
    val rawVotesData: java.net.URL = getClass.getResource("/dummy-election.responses.csv")
    val rawElectorateData: java.net.URL = getClass.getResource("/dummy-electorate.csv")

    CsvImport.importData(UrlSrc(rawVotesData), Some(UrlSrc(rawElectorateData)))
  }
}
