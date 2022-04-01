package com.gu.ballot.av

import cats.*
import cats.data.*
import com.gu.ballot.Candidate
import com.gu.ballot.*

/**
 * In an election, the slip of paper marked by the voter to denote their preferences is known as a
 * 'ballot', or, in British English (where 'ballot' refers to the entire voting system), a
 * 'ballot paper'.
 *
 * This class can denote the initial preferences from a 'ballot paper', as well as the subsequent
 * 'updated' preferences once eliminated candidates have been removed.
 */
case class Preference(order:NonEmptySeq[Candidate]) {
  {
    val repeatedCandidates = order.toSeq.frequencyCount.filter(_._2 > 1).keySet
    require(repeatedCandidates.isEmpty, s"Repeated candidates $repeatedCandidates in preferences $order")
  }

  def eliminate(candidates: Set[Candidate]): Option[Preference] = for {
    nonEmptyPreferenceOrder <- NonEmptySeq.fromSeq(order.filterNot(candidates.contains))
  } yield Preference(nonEmptyPreferenceOrder)
}

object Preference {
  def apply(first: Candidate, subsequent: Candidate*): Preference =
    Preference(NonEmptySeq(first, subsequent.toSeq))
}