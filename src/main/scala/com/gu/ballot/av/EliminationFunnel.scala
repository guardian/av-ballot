package com.gu.ballot.av

import cats.data.NonEmptySeq
import com.gu.ballot.{Candidate, VotesPerCandidate}
import com.gu.ballot.*

case class EliminationFunnel(stages: NonEmptySeq[VotesPerCandidate]) {
  def add(nextStage: VotesPerCandidate): EliminationFunnel = EliminationFunnel(stages :+ nextStage)

  val initialCandidates: Set[Candidate] = stages.head.candidates
  val ultimateCandidates: Set[Candidate] = stages.last.rankedByVotes.last.candidates

  val summary: String = {
    val stagesSummary = (for ((stage, index) <- stages.toSeq.zipWithOneBasedIndex.tail) yield {
      (s"${index.ordinal}-preference votes:" +: stage.rankText).map("\t"+_).mkString("\n")
    }).mkString("\n\n")

    s"""In order to decide who should be eliminated among the candidates tied for last-place
       |(${initialCandidates.andJoin}), subsequent-preference votes were checked
       |(only candidates being considered for elimination are listed):
       |
       |$stagesSummary""".stripMargin
  }
}
