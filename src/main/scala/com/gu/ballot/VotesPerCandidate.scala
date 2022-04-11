package com.gu.ballot

import cats.*
import cats.data.*
import cats.implicits.*
import scala.collection.immutable.SortedMap
import com.madgag.scala.collection.decorators.*

case class Acc(candidates: Set[Candidate], collectiveTotalVote: Int) {
  def add(moreCandidates: Set[Candidate], votesForEach: Int): Acc =
    Acc(candidates ++ moreCandidates, collectiveTotalVote + (votesForEach * moreCandidates.size))
}
object Acc {
  val Zero: Acc = Acc(Set.empty, 0)
}

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

  val candidatesThatCollectivelyHaveFewerVotesThanAnyOtherCandidate: Option[Acc] = {
    // find first rank where accumulated candidates leave fewer remaining votes than the individual rank vote

    val boo: Either[Acc, Acc] = rankedByVotes.toSeq.foldM(Acc.Zero) {
      case (acc, (individualVoteCountAtRank, rankGroup)) =>
        val updatedAcc = acc.add(rankGroup, individualVoteCountAtRank)
        val remainingVotes: Int = numVotes - updatedAcc.collectiveTotalVote
        val remainingVotesCouldChallenge = remainingVotes >= individualVoteCountAtRank
        Either.cond(remainingVotesCouldChallenge, updatedAcc, updatedAcc)
    }
    boo.left.toOption.map(invertAcc).filter(_.candidates.nonEmpty)
  }

  private def invertAcc(acc: Acc): Acc = Acc(votes.keySet -- acc.candidates, numVotes - acc.collectiveTotalVote)

  def add(candidate: Candidate, numVotes: Int): VotesPerCandidate = VotesPerCandidate(votes.updatedWith(candidate) {
    case Some(existingCount) => Some(existingCount + numVotes)
    case None => Some(numVotes)
  })

  def votesFor(candidateSubSet: Set[Candidate]): Int = candidateSubSet.flatMap(votes.get).sum

  def wouldHaveMajorityWith(num: Int): Boolean = num >= majorityThreshold
  def wouldHaveMajorityWith(candidateSubSet: Set[Candidate]): Boolean = wouldHaveMajorityWith(votesFor(candidateSubSet))

  /** @return It's possible that this group of candidates will collectively hold a *majority*
   * of the vote, if they are multiple tied candidates, rather than a single candidate!
   */
  def lastPlacedAmongst(candidateSubSet: Set[Candidate]): Set[Candidate] = {
    require(candidateSubSet.subsetOf(votes.keySet))

    VotesPerCandidate(votes.view.filterKeys(candidateSubSet).sumFrequencies).rankedByVotes.values.last
  }
}

object VotesPerCandidate {
  def zeroForAll(candidates: Set[Candidate]):VotesPerCandidate = {
    require(candidates.nonEmpty)
    VotesPerCandidate(candidates.map(_ -> 0).toMap)
  }
}
