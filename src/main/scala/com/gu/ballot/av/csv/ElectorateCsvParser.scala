package com.gu.ballot.av.csv

import com.gu.ballot.*
import kantan.csv.{CellDecoder, ReadError}
import cats.data.*
import cats.implicits.*

type ElectorateRow = (EmailAddress, String, GoogleGroup.Role)

object ElectorateCsvParser {
  def parse(csvSource: CsvSrc[ElectorateRow]): Electorate = {
    val rowResults = csvSource.inputSrc().toSeq
    val (headers, bodyRows) = rowResults.splitAt(2)

    val (headerRowsThatAreNotEmailAddresses, headerRowsThatSurprisinglyAreEmails) = headers.separate
    require(headerRowsThatAreNotEmailAddresses.size == 2)
    require(headerRowsThatSurprisinglyAreEmails.isEmpty)

    val (errors: Seq[ReadError], rows: Seq[ElectorateRow]) = bodyRows.separate
    require(errors.isEmpty)
    require(rows.nonEmpty, "Electorate must have some addresses!")

    println(s"Electorate file - number of addresses: ${rows.size}")

    val votingMembers = rows.filter(_._3 == GoogleGroup.Role.Member)

    println(s"Voting members: ${votingMembers.size}")

    Electorate(votingMembers.map(_._1).toSet)
  }

}
