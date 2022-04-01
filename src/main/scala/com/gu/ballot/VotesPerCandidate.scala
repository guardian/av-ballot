package com.gu.ballot

import scala.collection.immutable.SortedMap
import com.madgag.scala.collection.decorators.*

case class VotesPerCandidate(votes: Map[Candidate, Int]) {

  val numVotes:Int = votes.values.sum
  val hasVotes: Boolean = numVotes > 0
  val majorityThreshold:Int = 1+(numVotes/2)

  val rankedByVotes: SortedMap[Int, Set[Candidate]] =
    SortedMap.from(votes.groupUp(_._2)(_.keySet))(Ordering[Int].reverse)

  val clearWinner: Option[Candidate]= {
    val (leadingVoteNum, leadingCandidates) = rankedByVotes.head
    Option.when(leadingCandidates.size == 1 && leadingVoteNum > majorityThreshold)(leadingCandidates.head)
  }

  def add(candidate: Candidate, numVotes: Int): VotesPerCandidate = VotesPerCandidate(votes.updatedWith(candidate) {
    case Some(existingCount) => Some(existingCount + numVotes)
    case None => Some(numVotes)
  })

  def votesFor(candidateSubSet: Set[Candidate]) = candidateSubSet.flatMap(votes.get).sum

  def wouldHaveMajorityWith(num: Int): Boolean = num >= majorityThreshold
  def wouldHaveMajorityWith(candidateSubSet: Set[Candidate]): Boolean = wouldHaveMajorityWith(votesFor(candidateSubSet))

  def lastPlacedFrom(candidateSubSet: Set[Candidate]): Either[Set[Candidate], Set[Candidate]] = {
    val lastPlaced: Set[Candidate] = candidateSubSet.intersect(rankedByVotes.values.last)
    Either.cond(wouldHaveMajorityWith(lastPlaced), lastPlaced, lastPlaced)
  }
}

object VotesPerCandidate {
  def zeroForAll(candidates: Set[Candidate]):VotesPerCandidate = VotesPerCandidate(candidates.map(_ -> 0).toMap)
}
