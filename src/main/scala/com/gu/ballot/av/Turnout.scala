package com.gu.ballot.av

case class Turnout(numVotes: Int, electorateSize: Int) {
  require(numVotes >= 0)
  require(electorateSize > 0)
  require(numVotes <= electorateSize)

  val percentage: Float = (100f * numVotes) / electorateSize

  val summary: String =
    f"There were $numVotes votes from the $electorateSize eligible voters, giving a turnout of $percentage%2.1f%%."
}
