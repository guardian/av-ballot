package com.gu.ballot.av

import cats.*
import cats.data.*
import cats.implicits.*
import com.gu.ballot.*
import com.gu.ballot.Candidate
import com.madgag.scala.collection.decorators.*
import Round.Outcome.*
import com.gu.ballot.av.Round.{EliminationPath, TieResolution}

import scala.collection.immutable.SortedMap

type EliminationFunnel = NonEmptySeq[Set[Candidate]]

case class Round(countByPreference: Map[Preference, Int]) {

  val candidates:Set[Candidate] = countByPreference.keySet.flatMap(_.order.toSeq)

  val preferenceStages:LazyList[VotesPerCandidate] = LazyList.from(0).map { index =>
    countByPreference.foldLeft(VotesPerCandidate.zeroForAll(candidates)) {
      case (acc, (preference, count)) =>
        preference.order.get(index).fold(acc)(chosenCandidate => acc.add(chosenCandidate, count))
    }
  }.takeWhile(_.hasVotes)

  val firstPreferenceVotes:VotesPerCandidate = preferenceStages.head

  lazy val outcome: Round.Outcome = firstPreferenceVotes.clearWinner.map(ClearWinner(_)).getOrElse {
    // Elimination: Is there a strict subset of candidates (possibly 1 candidate) that collectively have fewer
    // votes than all the other voters? If so, yay - otherwise we have a lowest-rank tie, and need to try to
    // differentiate among the candidates in that lowest rank (which may be the only rank) to find the worst one.
    val borg: Either[Either[EssentialTie, TieResolution], EliminationPath] = firstPreferenceVotes.candidatesThatCollectivelyHaveFewerVotesThanAnyOtherCandidate.map { voo =>
      Round.FewerFirstPreferenceVotes(voo.candidates)
    }.toRight {
      val funnel: Either[EliminationFunnel, EliminationFunnel] =
        preferenceStages.foldM(NonEmptySeq.one(candidates)) {
          case (funnel, preferenceStage) =>
            val funnelStage: Set[Candidate] = preferenceStage.lastPlacedAmongst(funnel.last)
            val updatedFunnel = funnel.append(funnelStage)
            Either.cond(funnelStage.size == 1, funnel, funnel)
        }

      funnel.map(TieResolution(_)).left.map(EssentialTie(_))
    }
    borg.joinLeft.map(technique => Elimination(technique, eliminate(technique.eliminatedCandidates))).merge
  }

  def eliminate(candidates: Set[Candidate]): Round = Round((for {
    (preference, votes) <- countByPreference.toSeq
    updatedPreference <- preference.eliminate(candidates)
  } yield updatedPreference -> votes).sumFrequencies)
}

object Round {
  def apply(ballotPatterns: Preference*): Round = Round(ballotPatterns.frequencyCount)

  def apply(firstElement: (Preference, Int), otherElements: (Preference, Int)*): Round =
    Round((firstElement +: otherElements).toMap)

  enum Outcome:
    case ClearWinner(candidate: Candidate)
    case Elimination(eliminationPath: EliminationPath, nextRound: Round)
    case EssentialTie(failedTieBreakFunnel: EliminationFunnel) // No further candidates can be removed. No rational method can break an essential tie, only outside intervention


  sealed trait EliminationPath {
    val eliminatedCandidates: Set[Candidate]
  }

  /**
   * This is the elimination path that's most advantageous/clear in terms of reaching a resolution!
   *
   * From https://en.wikipedia.org/wiki/Instant-runoff_voting#Process :
   * "The set of all candidates with the fewest first-order votes whose votes together total less than
   *  any other candidate's can be eliminated without changing the outcome. This bulk elimination can
   *  bypass irrelevant ties"
   */
  case class FewerFirstPreferenceVotes(eliminatedCandidates: Set[Candidate]) extends EliminationPath

  // Show the narrowing set of last-placed candidates
  case class TieResolution(eliminationFunnel: EliminationFunnel) extends EliminationPath {
    override val eliminatedCandidates: Set[Candidate] = eliminationFunnel.last
  }

}