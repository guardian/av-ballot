package com.gu.ballot.av

import cats.*
import cats.data.*
import cats.implicits.*
import com.gu.ballot.*
import com.gu.ballot.Candidate
import com.madgag.scala.collection.decorators.*

import scala.collection.immutable.SortedMap

enum RoundOutcome:
  case ClearWinner(candidate: Candidate, candidatesByVote: SortedMap[Int, Set[Candidate]])
  case EssentialTie() // No further candidates can be removed. No rational method can break an essential tie, only outside intervention
  case LastPlacedCandidatesRemoved(candidatesRemoved: Set[Candidate]) // Show the narrowing set of last-placed candidates

case class Round(numberOfBallotsWithPreference: Map[Preference, Int]) {

  val candidates:Set[Candidate] = numberOfBallotsWithPreference.keySet.flatMap(_.order.toSeq)

  val preferenceStages:LazyList[VotesPerCandidate] = LazyList.from(0).map { index =>
    numberOfBallotsWithPreference.foldLeft(VotesPerCandidate.zeroForAll(candidates)) {
      case (acc, (preference, count)) =>
        preference.order.get(index).fold(acc)(chosenCandidate => acc.add(chosenCandidate, count))
    }
  }.takeWhile(_.hasVotes)

  val firstPreferenceVotes:VotesPerCandidate = preferenceStages.head

  val lastPlacedCandidates:Set[Candidate] = preferenceStages.foldM(candidates) {
    case (candidatesEligibleToBeLastPlaced, preferenceStage) =>
      preferenceStage.lastPlacedFrom(candidatesEligibleToBeLastPlaced)
  }.merge

  lazy val withoutLastPlacedCandidates: Round = Round((for {
    (preference, votes) <- numberOfBallotsWithPreference.toSeq
    updatedPreference <- preference.eliminate(lastPlacedCandidates)
  } yield updatedPreference -> votes).sumFrequencies)

  lazy val ultimateWinner:Candidate = // TODO - what if there are no more candidates to remove?!
    firstPreferenceVotes.clearWinner.getOrElse(withoutLastPlacedCandidates.ultimateWinner)
}

object Round {
  def apply(ballotPatterns: Preference*): Round = Round(ballotPatterns.frequencyCount)
}