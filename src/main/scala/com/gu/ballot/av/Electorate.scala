package com.gu.ballot.av

case class Electorate(emailAddresses: Set[String]) {
  val size: Int = emailAddresses.size
}
