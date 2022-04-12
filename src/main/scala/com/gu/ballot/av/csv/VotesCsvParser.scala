package com.gu.ballot.av.csv

import com.gu.ballot.*
import com.gu.ballot.av.{BallotCount, Preference}
import kantan.csv.*
import kantan.csv.ops.*
import cats.data.*
import cats.implicits.*
import com.gu.ballot.Candidate

object VotesCsvParser {
  def parse(file: java.io.File): BallotCount = {
    require(file.exists())
    val reader = file.asCsvReader[List[String]](rfc)

    val header: List[String] = reader.next().toOption.get

    val responses: (Seq[ReadError], Seq[List[String]]) = reader.toSeq.separate

    println(header)

    val voteRows: Seq[List[String]] = responses._2

    val columnsBeforeVoteColumns = 1

    val preferences: Seq[Preference] = for {
      voteRow <- voteRows
      prefSeq <- NonEmptySeq.fromSeq(voteRow.drop(columnsBeforeVoteColumns).filter(!_.isBlank).map(Candidate(_)))
    } yield Preference(prefSeq)

    BallotCount(preferences.frequencyCount)
  }
}
