package com.gu.ballot.av

import cats.data.NonEmptySeq
import com.gu.ballot.{Candidate, VotesPerCandidate}
import com.gu.ballot.*

/**
 * @param postFirstPreferenceStages the votes for candidates being considered for elimination, starting with
 *                                  2nd-preference votes
 */
case class EliminationFunnel(postFirstPreferenceStages: NonEmptySeq[VotesPerCandidate]) {
  def add(nextStage: VotesPerCandidate): EliminationFunnel = EliminationFunnel(postFirstPreferenceStages :+ nextStage)

  /**
   * The initialCandidates in an elimination funnel are solely the candidates that were tied-last-place
   * in a round.
   */
  val initialCandidates: Set[Candidate] = postFirstPreferenceStages.head.candidates

  /**
   * The ultimateCandidates in an elimination funnel are the candidates tied-last-place
   * in the consideration of the last stage of preferences. Hopefully just 1 candidate!
   */
  val ultimateCandidates: Set[Candidate] = postFirstPreferenceStages.last.rankedByVotes.last.candidates

  val summary: String = {
    val stagesSummary = (for ((stage, index) <- postFirstPreferenceStages.toSeq.zipWithOffsetIndex(2)) yield {
      (s"${index.ordinal}-preference votes:" +: stage.rankText).map("\t"+_).mkString("\n")
    }).mkString("\n\n")

    s"""In order to decide who should be eliminated among the candidates tied for last-place
       |(${initialCandidates.andJoin}), subsequent-preference votes were checked
       |(only candidates being considered for elimination are listed):
       |
       |$stagesSummary""".stripMargin
  }
}
