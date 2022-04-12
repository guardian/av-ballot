package com.gu.ballot.av.csv

import com.gu.ballot.*
import com.gu.ballot.av.{BallotCount, Preference}
import kantan.csv.*
import kantan.csv.ops.*
import cats.data.*
import cats.implicits.*
import com.gu.ballot.Candidate

object VotesCsvParser {
  def parse(csvSource: CsvSrc): BallotCount = {
    // require(file.exists())
    val reader = csvSource.inputSrc()

    val header: List[String] = reader.next().toOption.get
    val columnsBeforeVoteColumns = 2
    println("Vote preference columns are:\n\n"+header.drop(columnsBeforeVoteColumns).map(s => s"\t* $s").mkString("\n"))

    val responses: (Seq[ReadError], Seq[List[String]]) = reader.toSeq.separate


    val voteRows: Seq[List[String]] = responses._2

    val preferences: Seq[Preference] = for {
      voteRow <- voteRows
      prefSeq <- NonEmptySeq.fromSeq(voteRow.drop(columnsBeforeVoteColumns).filter(!_.isBlank).map(Candidate(_)))
    } yield Preference(prefSeq)

    BallotCount(preferences.frequencyCount)
  }
}
