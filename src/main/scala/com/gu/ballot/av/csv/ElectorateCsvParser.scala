package com.gu.ballot.av.csv

import com.gu.ballot.*
import kantan.csv.ReadError
import cats.data.*
import cats.implicits.*

object ElectorateCsvParser {
  def parse(csvSource: CsvSrc[EmailAddress]): Electorate = {
    val rowResults = csvSource.inputSrc().toSeq
    val (headers, bodyRows) = rowResults.splitAt(2)

    val (headerRowsThatAreNotEmailAddresses, headerRowsThatSurprisinglyAreEmails) = headers.separate
    require(headerRowsThatAreNotEmailAddresses.size == 2)
    require(headerRowsThatSurprisinglyAreEmails.isEmpty)

    val (errors: Seq[ReadError], addresses: Seq[EmailAddress]) = bodyRows.separate
    require(errors.isEmpty)
    require(addresses.nonEmpty, "Electorate must have some addresses!")

    println(s"Electorate file - number of addresses: ${addresses.size}")

    Electorate(addresses.toSet)
  }

}
