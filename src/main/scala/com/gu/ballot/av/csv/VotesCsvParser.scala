package com.gu.ballot.av.csv

import com.gu.ballot.*
import com.gu.ballot.av.{BallotCount, Preference}
import kantan.csv.*
import kantan.csv.ops.*
import cats.data.*
import cats.implicits.*
import com.gu.ballot.Candidate

object VotesCsvParser {

  def addressNormaliser(emailAddress: EmailAddress) = emailAddress.lowerCaseLocalPart

  def electorateNormaliser(electorate: Electorate): Set[String] = {
    val addressesById: Map[String, Set[EmailAddress]] = electorate.emailAddresses.groupBy(addressNormaliser)
    val worringlySimilarAddresses: Iterable[Set[EmailAddress]] = addressesById.values.filter(_.size > 2)
    require(worringlySimilarAddresses.isEmpty,
      s"There are email addresses that are too similar: ${worringlySimilarAddresses.mkString("\n")}")

    addressesById.keySet
  }

  def parse(votesCsvSrc: CsvSrc[List[String]], electorateOpt: Option[Electorate]): BallotCount = {
    val reader = votesCsvSrc.inputSrc()

    val header: List[String] = reader.next().toOption.get
    val columnsBeforeVoteColumns = 2
    println("Vote preference columns are:\n\n"+header.drop(columnsBeforeVoteColumns).map(s => s"\t* $s").mkString("\n"))

    val responses: (Seq[ReadError], Seq[List[String]]) = reader.toSeq.separate

    val voteRows: Seq[List[String]] = responses._2

    val legitimateVoteRows: Seq[List[String]] = electorateOpt.fold(voteRows) { electorate =>
      val normalisedElectorateIds = electorateNormaliser(electorate)
      val (eligible, ineligible) = voteRows.partition {
        row => normalisedElectorateIds.contains(addressNormaliser(EmailAddress(row(1))))
      }

      if (ineligible.isEmpty) {
        println("All votes were by eligible voters, no votes discarded.")
      } else {
        val ineligibleVoterAddresses = ineligible.map(_.apply(1)).sorted
        println(s"${ineligible.size} votes by ineligible voters:\n\n${ineligibleVoterAddresses.mkString("\n")}")
      }

      eligible
    }

    val preferences: Seq[Preference] = for {
      voteRow <- legitimateVoteRows
      prefSeq <- NonEmptySeq.fromSeq(voteRow.drop(columnsBeforeVoteColumns).filter(!_.isBlank).map(Candidate(_)))
    } yield Preference(prefSeq)

    BallotCount(preferences.frequencyCount)
  }
}
