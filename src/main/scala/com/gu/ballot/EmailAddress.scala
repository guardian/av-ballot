package com.gu.ballot

import kantan.csv._
import kantan.csv.ops._

case class EmailAddress(address: String) {
  require(address.contains("@"))

  val Array(localPart, domain) = address.split('@')
  
  val lowerCaseLocalPart: String = localPart.toLowerCase
}

object EmailAddress {
  implicit val emailAddressCsvCodec: CellCodec[EmailAddress] = {
    CellCodec.from(s => DecodeResult(EmailAddress(s)))(d => d.address)
  }
}