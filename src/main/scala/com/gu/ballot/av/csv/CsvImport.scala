package com.gu.ballot.av.csv

import com.gu.ballot.{Electorate, EmailAddress}
import com.gu.ballot.av.BallotReport
import com.gu.ballot.av.csv.CsvSrc.FileSrc

import java.io.File

object CsvImport {
  def importData(votesCsv: CsvSrc[List[String]], electorateFile: Option[CsvSrc[ElectorateRow]] = None): BallotReport = {
    val electorateOpt: Option[Electorate] =
      for (electorateFile <- electorateFile) yield ElectorateCsvParser.parse(electorateFile)

    val ballotCount = VotesCsvParser.parse(votesCsv, electorateOpt)

    val ballotReport = electorateOpt.fold(BallotReport.derivingElectorateFrom(ballotCount)) {
      electorate => BallotReport(electorateSize = electorate.emailAddresses.size, ballotCount = ballotCount)
    }
    println("\n\n"+ballotReport.report)

    ballotReport
  }
}
