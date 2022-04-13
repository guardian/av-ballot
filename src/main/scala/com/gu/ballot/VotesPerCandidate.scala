package com.gu.ballot

import cats.*
import cats.data.*
import cats.implicits.*
import com.gu.ballot.VotesPerCandidate.{TotalVotes, VoteRank}
import com.gu.ballot.VotesPerCandidate.VoteRank.SingleCandidate

import scala.collection.immutable.{SortedMap, SortedSet}
import com.madgag.scala.collection.decorators.*

case class Acc(candidates: Set[Candidate], collectiveTotalVote: Int) {
  def add(voteRank: VoteRank): Acc =
    Acc(candidates ++ voteRank.candidates, collectiveTotalVote + voteRank.totalRankVotes)
}
object Acc {
  val Zero: Acc = Acc(Set.empty, 0)
}

case class VotesPerCandidate(votes: Map[Candidate, Int]) {

  val numVotes:Int = votes.values.sum
  val hasVotes: Boolean = numVotes > 0
  val majorityThreshold:Int = 1+(numVotes/2)
  
  val candidates: Set[Candidate] = votes.keySet

  val rankedByVotes: SortedSet[VoteRank] = SortedSet.from(for {
    (individualVotes, rankGroup) <- votes.groupUp(_._2)(_.keySet)
  } yield VoteRank(rankGroup, individualVotes))

  val hasSingleLastPlacedCandidate: Boolean = rankedByVotes.last.candidates.size == 1

  val clearWinner: Option[Candidate] = rankedByVotes.head match {
    case voteRank: SingleCandidate if voteRank.individualVotes >= majorityThreshold => Some(voteRank.candidate)
    case _ => None
  }

  val candidatesThatCollectivelyHaveFewerVotesThanAnyOtherCandidate: Option[Acc] = {
    // find first rank where accumulated candidates leave fewer remaining votes than the individual rank vote

    rankedByVotes.toSeq.foldM(Acc.Zero) {
      case (acc, voteRank) =>
        val updatedAcc = acc.add(voteRank)
        val remainingVotes: Int = numVotes - updatedAcc.collectiveTotalVote
        val remainingVotesCouldChallenge = remainingVotes >= voteRank.individualVotes
        Either.cond(remainingVotesCouldChallenge, updatedAcc, updatedAcc)
    }.left.toOption.map(invertAcc).filter(_.candidates.nonEmpty)
  }

  private def invertAcc(acc: Acc): Acc = Acc(votes.keySet -- acc.candidates, numVotes - acc.collectiveTotalVote)

  def rankText(using totalVotes: TotalVotes = TotalVotes(numVotes)): Seq[String] = {
    require(totalVotes.num >= numVotes)

    OneBasedList(rankedByVotes.toSeq).numberedList(_.summary)
  }

  def add(candidate: Candidate, numVotes: Int): VotesPerCandidate = VotesPerCandidate(votes.updatedWith(candidate) {
    case Some(existingCount) => Some(existingCount + numVotes)
    case None => Some(numVotes)
  })

  def votesFor(candidateSubSet: Set[Candidate]): Int = candidateSubSet.toSeq.flatMap(votes.get).sum

  def subSetFor(candidateSubSet: Set[Candidate]): VotesPerCandidate = VotesPerCandidate(
    votes.view.filterKeys(candidateSubSet).toMap
  )

  def forLastRankedCandidates: VotesPerCandidate = subSetFor(rankedByVotes.last.candidates)

//  /** @return It's possible that this group of candidates will collectively hold a *majority*
//   * of the vote, if they are multiple tied candidates, rather than a single candidate!
//   */
//  def lastPlacedAmongst(candidateSubSet: Set[Candidate]): VotesPerCandidate = {
//    require(candidateSubSet.subsetOf(votes.keySet))
//
//    VotesPerCandidate(votes.view.filterKeys(candidateSubSet).toMap)
//  }

  def wouldHaveMajorityWith(num: Int): Boolean = num >= majorityThreshold
  def wouldHaveMajorityWith(candidateSubSet: Set[Candidate]): Boolean = wouldHaveMajorityWith(votesFor(candidateSubSet))


}

object VotesPerCandidate {
  def zeroForAll(candidates: Set[Candidate]):VotesPerCandidate = {
    require(candidates.nonEmpty)
    VotesPerCandidate(candidates.map(_ -> 0).toMap)
  }

  sealed trait VoteRank {
    val individualVotes: Int
    val totalRankVotes: Int
    val candidates: Set[Candidate]
    def summary(using TotalVotes): String
  }

  object VoteRank {
    given Ordering[VoteRank] = Ordering[Int].reverse.on(_.individualVotes)

    def apply(candidates: Set[Candidate], individualVote: Int): VoteRank = {
      if (candidates.size == 1) SingleCandidate(candidates.head, individualVote) else Tie(candidates, individualVote)
    }

    case class SingleCandidate(candidate: Candidate, individualVotes: Int) extends VoteRank {
      val totalRankVotes: Int = individualVotes
      val candidates: Set[Candidate] = Set(candidate)
      def summary(using TotalVotes): String = s"$candidate: ${individualVotes.summary}"
    }
    case class Tie(candidates: Set[Candidate], individualVotes: Int) extends VoteRank {
      val totalRankVotes: Int = individualVotes * candidates.size
      def summary(using TotalVotes): String = s"[Tie] ${candidates.mkString(", ")}: ${individualVotes.summary} each, ${totalRankVotes.summary} total"
        // [Tie] Whitebeam, Sycamore, Western Red Cedar: 50 votes (17%) each, 150 votes (51%) total
    }
  }

  case class TotalVotes(num: Int)

  extension (voteCount: Int)
    def summary(using t: TotalVotes): String = {
      val percentage = (100f * voteCount)/t.num
      f"$voteCount votes ($percentage%2.1f%%)"
    }
}
