package com.gu.ballot.av

import cats.*
import cats.data.*
import cats.implicits.*
import com.gu.ballot.*
import com.gu.ballot.Candidate
import com.madgag.scala.collection.decorators.*
import Round.Outcome.*
import com.gu.ballot.av.Round.Outcome.Elimination.{FewerFirstPreferenceVotes, TieResolution}

import scala.collection.immutable.SortedMap

case class Round(countByPreference: Map[Preference, Int]) {
  
  val candidates:Set[Candidate] = countByPreference.keySet.flatMap(_.order.toSeq)

  require(candidates.nonEmpty)

  val preferenceStages:LazyList[VotesPerCandidate] = LazyList.from(0).map { index =>
    countByPreference.foldLeft(VotesPerCandidate.zeroForAll(candidates)) {
      case (acc, (preference, count)) =>
        preference.order.get(index).fold(acc)(chosenCandidate => acc.add(chosenCandidate, count))
    }
  }.takeWhile(_.hasVotes)

  val firstPreferenceVotes:VotesPerCandidate = preferenceStages.head

  val nonTransferableVotes: Int =
    preferenceStages.head.numVotes - preferenceStages.tail.headOption.map(_.numVotes).getOrElse(0)

  lazy val outcome: Round.Outcome = firstPreferenceVotes.clearWinner.map(ClearWinner(_)).getOrElse {
    // Elimination: Is there a strict subset of candidates (possibly 1 candidate) that collectively have fewer
    // votes than all the other voters? If so, yay - otherwise we have a lowest-rank tie, and need to try to
    // differentiate among the candidates in that lowest rank (which may be the only rank) to find the worst one.
    val borg: Either[Either[EssentialTie, TieResolution], Elimination.Approach] =
    firstPreferenceVotes.candidatesThatCollectivelyHaveFewerVotesThanAnyOtherCandidate.map { voo =>
      FewerFirstPreferenceVotes(voo.candidates)
    }.toRight {
      val funnel: EliminationFunnel =
        preferenceStages.tail.foldM(EliminationFunnel(NonEmptySeq.one(firstPreferenceVotes.forLastRankedCandidates))) {
          case (funnel, preferenceStage) =>
            val funnelStage: VotesPerCandidate = preferenceStage.subSetFor(funnel.stages.last.rankedByVotes.last.candidates)
            val updatedFunnel = funnel.add(funnelStage)
            Either.cond(funnelStage.hasSingleCandidateRemaining, updatedFunnel, updatedFunnel)
        }.merge

      Either.cond(funnel.ultimateCandidates.size < candidates.size, TieResolution(funnel), EssentialTie(funnel))
    }
    borg.joinLeft.map(technique => Elimination(technique, eliminate(technique.eliminatedCandidates))).merge
  }

  lazy val outcomeSummary: String = outcome match {
    case ClearWinner(winner) => s"with a majority of xx%, $winner was declared winner"
    case elimination: Elimination => s"${elimination.eliminatedCandidates.wasOrWere} eliminated"
    case essentialTie: EssentialTie => "there was an essential tie."
  }

  lazy val nextRound: Option[Round] = outcome match {
    case e: Elimination => Some(e.nextRound)
    case _ => None
  }
  
  lazy val ultimateConclusion: Round.Conclusion = outcome match {
    case c: Round.Conclusion => c
    case elimination: Elimination => elimination.nextRound.ultimateConclusion
  }

  def eliminate(candidatesToEliminate: Set[Candidate]): Round = {
    require(candidatesToEliminate.nonEmpty)
    require(candidatesToEliminate != candidates)
    Round((for {
      (preference, votes) <- countByPreference.toSeq
      updatedPreference <- preference.eliminate(candidatesToEliminate)
    } yield updatedPreference -> votes).sumFrequencies)
  }

  val summary: String = (Seq(
    s"the ${firstPreferenceVotes.numVotes} votes ($nonTransferableVotes non-transferable) were as follows:",
    firstPreferenceVotes.rankText.mkString("\n"),
  ) ++
    outcome.furtherExplanation(using firstPreferenceVotes)
    ++ Seq(s"Consequently, $outcomeSummary")).mkString("\n\n")
}

object Round {
  def apply(ballotPatterns: Preference*): Round = Round(ballotPatterns.frequencyCount)

  def apply(firstElement: (Preference, Int), otherElements: (Preference, Int)*): Round =
    Round((firstElement +: otherElements).toMap)

  sealed trait Outcome {
    def furtherExplanation(using VotesPerCandidate): Option[String]
  }
  sealed trait Conclusion extends Outcome

  object Outcome {

    case class ClearWinner(candidate: Candidate) extends Conclusion {
      def furtherExplanation(using VotesPerCandidate): Option[String] = None
    }
    case class EssentialTie(failedTieBreakFunnel: EliminationFunnel) extends Conclusion {
      def furtherExplanation(using VotesPerCandidate): Option[String] = Some {
        s"No way to resolve the Essential Tie - could not differentiate candidates: ${failedTieBreakFunnel}"
      }
    }
    case class Elimination(approach: Elimination.Approach, nextRound: Round) extends Outcome {
      val eliminatedCandidates: Set[Candidate] = approach.eliminatedCandidates
      def furtherExplanation(using VotesPerCandidate): Option[String] = approach.explanation
    }

    object Elimination {
      sealed trait Approach {
        val eliminatedCandidates: Set[Candidate]

        def explanation(using votes: VotesPerCandidate): Option[String]
      }

      /**
       * This is the elimination path that's most inarguable/clear in terms of reaching a resolution!
       *
       * From https://en.wikipedia.org/wiki/Instant-runoff_voting#Process :
       * "The set of all candidates with the fewest first-order votes whose votes together total less than
       *  any other candidate's can be eliminated without changing the outcome. This bulk elimination can
       *  bypass irrelevant ties"
       */
      case class FewerFirstPreferenceVotes(eliminatedCandidates: Set[Candidate]) extends Approach {
        override def explanation(using votes: VotesPerCandidate): Option[String] = Option.when(eliminatedCandidates.size > 1) {
          s"Collectively, candidates ${eliminatedCandidates.andJoin} held only ${votes.votesFor(eliminatedCandidates)} votes, " +
            s"fewer than any other candidate."
        }
      }

      case class TieResolution(eliminationFunnel: EliminationFunnel) extends Approach {
        override val eliminatedCandidates: Set[Candidate] = eliminationFunnel.ultimateCandidates
        override def explanation(using votes: VotesPerCandidate): Option[String] = Some(eliminationFunnel.summary)
      }
    }
  }
}